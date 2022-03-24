# temperature quantile plots
# based on tempmodels5.Rmd

# TODO:
  # use initial2 to record inputs, get rid of read + write .txt files
  # if the above does not work, turn interactive to a "submit" button

# fit.rq = readRDS(file = "quantileModel2.RDS")
# initial = read.table("currentInput.txt", header=TRUE)
# # initial2 = read.table("currentInput2.txt", header=TRUE)
# rangeData = read.table("rangeData.txt", header=TRUE) # range of training data
# read.table("/Users/zijungao/Desktop/Research/Trevor/temperature/code/currentInput.txt", header = TRUE)

library("dplyr")
library("shiny")
library("ggplot2")
library("ggthemes")
library("plotly")
library("quantreg")
library("splines")

ui = fluidPage(
  headerPanel("Personalized Temperature Range Chart"),
  headerPanel(""),
  # sidebar for inputs
  sidebarPanel(width = 5,
    fluidRow(
      column(4, style='padding:0px;',
        radioButtons(inputId = "gender", label = "Sex", choices = c("Female" = "female", "Male" = "male"), selected = temperature:::initial$sex, width = "400px", inline = FALSE)
      ),
      column(7, style='padding:0px;',
        numericInput(inputId = "age", label = "Age", value = temperature:::initial$age, min = 0, max = 200, width = "400px")
      ),
    ),

    fluidRow(
      column(4, style='padding:0px;',
        radioButtons(inputId = "sizeUnit", label = "Size Units", choices = c("Metric" = "metric", "American" = "American"), selected = temperature:::initial$sizeUnit, width = "400px", inline = FALSE)
      ),
      column(4, style='padding:0px;',
        # only show this panel if the Metric unit is used
        conditionalPanel(
          condition = "input.sizeUnit == 'metric'",
          column(1.6, offset = 0, style='padding:0px;',
            numericInput(inputId = "height", label = "Height (m)", value = round(temperature:::initial$height, digits = 2), min = 0, max = 4, width = "100px")
          )
        ),
        # only show this panel if the American unit is used
        conditionalPanel(
          condition = "input.sizeUnit == 'American'",
          column(1.6, style='padding:0px;',
            numericInput(inputId = "height1", label = "Height (ft)", value = format(round(temperature:::initial$height, digits = 0), nsmall = 0), min = 0, max = 10, width = "100px")
          ),
          column(1.6, offset = 0, style='padding:0px;',
            numericInput(inputId = "height2", label = "(in)", value = format(round(temperature:::initial$height2, digits = 0), nsmall = 0), min = 0, max = 20, width = "100px")
          )
        )
      ),
      column(4, style='padding:0.0px;',
        numericInput(inputId = "weight", label = "Weight", value = round(temperature:::initial$weight, digits = 1), min = 0, max = 800, width = "100px")
      )
    ),
    fluidRow(
      column(4, style='padding:0px;',
        radioButtons(inputId = "tempUnit", label = "Temperature Units", choices = c( "\u00B0F" = "FD", "\u00B0C" = "CD"), selected = temperature:::initial$tempUnit, width = "400px", inline = FALSE)
      ),
      column(7, style='padding:0px;',
        numericInput(inputId = "temp", label = "Temperature", value = "", min = 0, max = 800, width = "400px") # value = round(temperature:::initial$temp, digits = 1)
      )
    ),
    fluidRow(
      column(11, style='padding:0px;',
        # verbatimTextOutput("local"),
        # extract user's local time
        HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
        HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
        tags$script('
                    $(function() {
                    var time_now = new Date()
                    $("input#client_time").val(time_now.getTime())
                    $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
                    });
        ')
      )
    )
  ),
  mainPanel(width = 7,
    plotlyOutput(outputId = "quantilePlot")
  )
)

server = function(input, output, session){
  output$quantilePlot = renderPlotly({
    client_time <- reactive(as.numeric(input$client_time) / 1000) # in s
    time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) * 60 ) # in s
    currentTime = reactive((client_time() - time_zone_offset()) %% 86400)
    # validate(need(currentTime() >= (7*3600), "Please try between 7:00 and 18:00"))
    # validate(need(currentTime() <= (18*3600), "Please try between 7:00 and 18:00"))
    # output$local = renderText(currentTime()) # debug
    currentTimeIndex = round(((currentTime()/60 + 17 * 60)/15) %% 96) + 1
    if(currentTimeIndex > 68){currentTimeIndex = 1
    } else if(currentTimeIndex > 45){currentTimeIndex = 45}
    # output$local = renderText(currentTimeIndex) # debug

    # check inputs
    validate(need(input$gender != "", "Please input your gender"))
    validate(need(input$age != "", "Please input your age"))
    validate(need(input$age >= temperature:::rangeData$age[1], "Please input a propoer age"))
    validate(need(input$age <= temperature:::rangeData$age[2], "Please input a propoer age"))
    validate(need(input$weight != "", "Please input your weight"))
    if(input$sizeUnit == "American"){
      validate(need(input$height1 != "", "Please input a propoer height"))
      if(!is.na(input$height1)){
        validate(need(input$height2 != "", "Please input a propoer height"))
      }
      updateNumericInput(session, "weight", label = "Weight (lb)", value = format(round(input$weight, digits = 1), nsmall = 1))
    } else if(input$sizeUnit == "metric"){
      validate(need(input$height != "", "Please input a propoer height"))
      updateNumericInput(session, "weight", label = "Weight (kg)", value = format(round(input$weight, digits = 1), nsmall = 1))
    }

    temp = read.table("currentInput.txt", header=TRUE)
    if(temp$sizeUnit != input$sizeUnit){
      if(temp$sizeUnit == "metric"){
        temp2 = temp; temp2$sizeUnit = input$sizeUnit; temp2$weight = temp$weight/0.453592; temp2$height = floor(temp$height/0.3048); temp2$height2 = round(12 * (temp$height/0.3048 - floor(temp$height/0.3048)))
        write.table(temp2, "currentInput.txt")
        updateNumericInput(session, "weight", value = format(round(temp2$weight, digits = 1), nsmall = 1))
        updateNumericInput(session, "height1", value = format(round(temp2$height, digits = 0), nsmall = 0))
        updateNumericInput(session, "height2", value = format(round(temp2$height2, digits = 1), nsmall = 1))
      } else if(temp$sizeUnit == "American"){
        temp2 = temp; temp2$sizeUnit = input$sizeUnit; temp2$weight = temp$weight*0.453592; temp2$height = (temp$height + temp$height2/12)*0.3048; temp2$height2 = 0
        write.table(temp2, "currentInput.txt")
        updateNumericInput(session, "weight", value = format(round(temp2$weight, digits = 1), nsmall = 1))
        updateNumericInput(session, "height", value = format(round(temp2$height, digits = 2), nsmall = 2))
      }
    } else if(temp$tempUnit != input$tempUnit){
      if(temp$tempUnit == "FD"){
        temp2 = temp; temp2$tempUnit = "CD"; temp2$temp = (temp$temp - 32) * 5/9
        write.table(temp2, "currentInput.txt")
        if(!is.na(input$temp)){
          updateNumericInput(session, "temp", value = format(round(temp2$temp, digits = 1), nsmall = 1))
        }
      } else if(temp$tempUnit == "CD"){
        temp2 = temp; temp2$tempUnit = "FD"; temp2$temp = temp$temp * 9/5 + 32
        write.table(temp2, "currentInput.txt")
        if(!is.na(input$temp)){
          updateNumericInput(session, "temp", value = format(round(temp2$temp, digits = 1), nsmall = 1))
        }
      }
    } else{
      temp$sex = input$sex; temp$age = input$age; temp$weight = input$weight;
      if(!is.na(input$temp)){temp$temp = input$temp}
      if(input$sizeUnit == "metric"){
        temp$height = input$height
      } else if (input$sizeUnit == "American"){
        temp$height = input$height1; temp$height2 = input$height2
      }
      write.table(temp, "currentInput.txt")
    }

    temp = read.table("currentInput.txt", header=TRUE)
    if(temp$sizeUnit == "American"){
      weight = temp$weight * 0.453592
      height = (temp$height + temp$height2/12) * 0.3048
    } else if(temp$sizeUnit == "metric"){
      weight = temp$weight
      height = temp$height
    }
    # range
    validate(need(height >= temperature:::rangeData$height[1], "Please input a propoer height"))
    validate(need(height <= temperature:::rangeData$height[2], "Please input a propoer height"))
    validate(need(weight >= temperature:::rangeData$weight[1], "Please input a propoer weight"))
    validate(need(weight <= temperature:::rangeData$weight[2], "Please input a propoer weight"))

    # quantile model
    if(input$gender == "male"){
      gender.plus = "male"
    } else if(input$age < 40){
      gender.plus = "female.pre"
    } else {
      gender.plus = "female.post"
    }
    pred_data=expand.grid(gender.plus=gender.plus, age_yrs=input$age,height_m=height,weight_kg=weight,TIME_HOUR=seq(7,18,by = 0.25))

    # plot
    if(input$tempUnit == "FD"){
      preds=round(predict(temperature:::fit.rq,pred_data), digits = 2)
    } else if(input$tempUnit == "CD"){
      preds=round((predict(temperature:::fit.rq,pred_data) - 32) * 5/9, digits = 2)
    }
    start = hms::as_hms("00:00:00")
    time =  hms::as_hms(start +  seq(3600 * 7, 3600 * 18, by = 15 * 60))
    plotData = data.frame(time, preds); colnames(plotData) = c("Time", "quantile1", "quantile5", "quantile25", "quantile50", "quantile75", "quantile95", "quantile99")
    plotData$quantile1Name = NA; plotData$quantile1Name[dim(plotData)[1]] = "1%"
    plotData$quantile5Name = NA; plotData$quantile5Name[dim(plotData)[1]] = "5%"
    plotData$quantile25Name = NA; plotData$quantile25Name[dim(plotData)[1]] = "25%"
    plotData$quantile50Name = NA; plotData$quantile50Name[dim(plotData)[1]] = "50%"
    plotData$quantile75Name = NA; plotData$quantile75Name[dim(plotData)[1]] = "75%"
    plotData$quantile95Name = NA; plotData$quantile95Name[dim(plotData)[1]] = "95%"
    plotData$quantile99Name = NA; plotData$quantile99Name[dim(plotData)[1]] = "99%"
    if(!is.na(input$temp)){
      if(temp$temp > plotData$quantile99[currentTimeIndex]){
        currPercentile = ">99%"
      } else if(temp$temp > plotData$quantile95[currentTimeIndex]){
        currPercentile = paste(round((95 * (plotData$quantile99[currentTimeIndex] - temp$temp)+ 99 * (temp$temp - plotData$quantile95[currentTimeIndex]))/(plotData$quantile99[currentTimeIndex] - plotData$quantile95[currentTimeIndex])), "%", sep = "")
      } else if(temp$temp > plotData$quantile75[currentTimeIndex]){
        currPercentile = paste(round((75 * (plotData$quantile95[currentTimeIndex] - temp$temp)+ 95 * (temp$temp - plotData$quantile75[currentTimeIndex]))/(plotData$quantile95[currentTimeIndex] - plotData$quantile75[currentTimeIndex])), "%", sep = "")
      } else if(temp$temp > plotData$quantile50[currentTimeIndex]){
        currPercentile = paste(round((50 * (plotData$quantile75[currentTimeIndex] - temp$temp)+ 75 * (temp$temp - plotData$quantile50[currentTimeIndex]))/(plotData$quantile75[currentTimeIndex] - plotData$quantile50[currentTimeIndex])), "%", sep = "")
      } else if(temp$temp > plotData$quantile25[currentTimeIndex]){
        currPercentile = paste(round((25 * (plotData$quantile50[currentTimeIndex] - temp$temp)+ 50 * (temp$temp - plotData$quantile25[currentTimeIndex]))/(plotData$quantile50[currentTimeIndex] - plotData$quantile25[currentTimeIndex])), "%", sep = "")
      } else if(temp$temp > plotData$quantile5[currentTimeIndex]){
        currPercentile = paste(round((5 * (plotData$quantile25[currentTimeIndex] - temp$temp)+ 25 * (temp$temp - plotData$quantile5[currentTimeIndex]))/(plotData$quantile25[currentTimeIndex] - plotData$quantile5[currentTimeIndex])), "%", sep = "")
      } else if(temp$temp > plotData$quantile1[currentTimeIndex]){
        currPercentile = paste(round((1 * (plotData$quantile5[currentTimeIndex] - temp$temp)+ 5 * (temp$temp - plotData$quantile1[currentTimeIndex]))/(plotData$quantile5[currentTimeIndex] - plotData$quantile1[currentTimeIndex])), "%", sep = "")
      } else{currPercentile = "<1%"}
      plotData$currentTemp = NA; plotData$currentTemp[currentTimeIndex] = temp$temp
      x = list(title = list(text = "<b> Time <b>", standoff = 20), titlefont = "f", autotick = FALSE, dtick = 8, tickformat="%H:%M:%OS3", tickangle = 0, range = c(-0.5,44.5))
      if(input$tempUnit == "FD"){
        y = list(title = list(text = "<b> Temperature (\u00B0F) <b>", standoff = 30), titlefont = "f", range = c(95.8,99.9), nsmall=2, dtick = 0.4, tickformat = ".1f");
        title = list(text = paste("<b>", "Temperature: ", format(round(temp$temp, digits = 1), nsmall = 1), "\u00B0F; ", "Percentile: ", format(currPercentile, nsmall = 0), "<b>", sep = ""))
      } else if(input$tempUnit == "CD"){
        y = list(title = list(text = "<b> Temperature (\u00B0C) <b>", standoff = 30), titlefont = "f", range = c(35.44444,37.72222), dtick = 0.2,  tickformat = ".1f")
        title = list(text = paste("<b>", "Temperature: ", format(round(temp$temp, digits = 1), nsmall = 1), "\u00B0C; ", "Percentile: ", format(currPercentile, nsmall = 0), "<b>", sep = ""))
      }
      fig <- plot_ly(plotData, x = ~Time, y = ~quantile99, name = '99%', type = 'scatter', mode = 'lines', line = list(color = rgb(1,0,0,1), width = 1, dash = "solid"), height = 400)
      fig <- fig %>% add_trace(y = ~quantile95, name = '95%', mode = 'lines', line =list(color = rgb(1,0,0,1), width = 1.5, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile75, name = '75%', mode = 'lines', line =list(color = rgb(1,0,0,1), width = 2, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile50, name = '50%', mode = 'lines', line =list(color = rgb(0.5,0,0.5,1), width = 3, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile25, name = '25%', mode = 'lines', line =list(color = rgb(0,0,1,1), width = 2, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile5, name = '5%', mode = 'lines', line =list(color = rgb(0,0,1,1), width = 1.5, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile1, name = '1%', mode = 'lines', line =list(color = rgb(0,0,1,1), width = 1, dash = "solid"))
      fig <- fig %>% add_trace(data = filter(plotData, !is.na(currentTemp)), y = ~ currentTemp, name = 'TEMP', mode = 'lines + markers', marker = list(color = "black", size = 10, opacity = 1), line = list(color = "white", width = 3, dash = "dot", opacity = 0))
      # annotations of quantiles
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile1[45], xref = "x", yref = "y", text = "1%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile5[45], xref = "x", yref = "y", text = "5%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile25[45], xref = "x", yref = "y", text = "25%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile50[45], xref = "x", yref = "y", text = "50%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile75[45], xref = "x", yref = "y", text = "75%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile95[45], xref = "x", yref = "y", text = "95%", xanchor = 'left',showarrow = F,  textposition = 'outside')
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile99[45], xref = "x", yref = "y", text = "99%", xanchor = 'left',showarrow = F,  textposition = 'outside')
      fig <- fig %>% layout(title = title, hovermode = "x unified", xaxis = x, yaxis = y, showlegend = FALSE, margin = list(l = 0, r = 50, b = 0, t = 35), font = list(size = 14))
    } else {
      x = list(title = list(text = "<b> Time <b>", standoff = 20), titlefont = "f", autotick = FALSE, dtick = 8, tickformat="%H:%M:%OS3", tickangle = 0, range = c(-0.5,44.5))
      if(input$tempUnit == "FD"){
        y = list(title = list(text = "<b> Temperature (\u00B0F) <b>", standoff = 30), titlefont = "f", range = c(95.8,99.9), nsmall=2, dtick = 0.4, tickformat = ".1f");
        title = list(text = paste("<b>", "Temperature: NA; Percentile: NA", "<b>", sep = ""))
      } else if(input$tempUnit == "CD"){
        y = list(title = list(text = "<b> Temperature (\u00B0C) <b>", standoff = 30), titlefont = "f", range = c(35.44444,37.72222), dtick = 0.2,  tickformat = ".1f")
        title = list(text = paste("<b>", "Temperature: NA; Percentile: NA", "<b>", sep = ""))
      }
      fig <- plot_ly(plotData, x = ~Time, y = ~quantile99, name = '99%', type = 'scatter', mode = 'lines', line = list(color = rgb(1,0,0,1), width = 1, dash = "solid"), height = 400) # , width = 500, height = 500
      fig <- fig %>% add_trace(y = ~quantile95, name = '95%', mode = 'lines', line =list(color = rgb(1,0,0,1), width = 1.5, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile75, name = '75%', mode = 'lines', line =list(color = rgb(1,0,0, 1), width = 2, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile50, name = '50%', mode = 'lines', line =list(color = rgb(1/2,0,1/2,1), width = 3, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile25, name = '25%', mode = 'lines', line =list(color = rgb(0,0,1,1), width = 2, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile5, name = '5%', mode = 'lines', line =list(color = rgb(0,0,1,1), width = 1.5, dash = "solid"))
      fig <- fig %>% add_trace(y = ~quantile1, name = '1%', mode = 'lines', line =list(color = rgb(0,0,1,1), width = 1, dash = "solid"))

      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile1[45], xref = "x", yref = "y", text = "1%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile5[45], xref = "x", yref = "y", text = "5%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile25[45], xref = "x", yref = "y", text = "25%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile50[45], xref = "x", yref = "y", text = "50%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile75[45], xref = "x", yref = "y", text = "75%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile95[45], xref = "x", yref = "y", text = "95%", xanchor = 'left',showarrow = F)
      fig <- fig %>% add_annotations(x = (plotData$Time[45] - plotData$Time[1])/60/15, y = plotData$quantile99[45], xref = "x", yref = "y", text = "99%", xanchor = 'left',showarrow = F,  textposition = 'outside')

      fig <- fig %>% layout(title = title, hovermode = "x unified", xaxis = x, yaxis = y, showlegend = FALSE, margin = list(l = 0, r = 50, b = 0, t = 35), font = list(size = 14))
    }
  })
}

shinyApp(ui = ui, server = server)

