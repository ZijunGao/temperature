# visualization of body temperature quantiles
ui = fluidPage(
  headerPanel("Personalized Temperature Range Chart"),
  headerPanel(""),
  # sidebar for inputs
  sidebarPanel(width = 5,
    fluidRow(
      column(4, style='padding:0px;',
        radioButtons(inputId = "gender", label = "Sex", choices = c("Female" = "female", "Male" = "male"), selected = initial$sex, width = "400px", inline = FALSE)
      ),
      column(7, style='padding:0px;',
        numericInput(inputId = "age", label = "Age", value = initial$age, min = 0, max = 200, width = "400px")
      ),
    ),

    fluidRow(
      column(4, style='padding:0px;',
        radioButtons(inputId = "sizeUnit", label = "Size Unit", choices = c("Metric" = "metric", "American" = "American"), selected = initial$sizeUnit, width = "400px", inline = FALSE)
      ),
      column(4, style='padding:0px;',
        # only show this panel if the Metric unit is used
        conditionalPanel(
          condition = "input.sizeUnit == 'metric'",
          column(1.6, offset = 0, style='padding:0px;',
            numericInput(inputId = "height", label = "Height (m)", value = initial$height, min = 0, max = 4, step = 0.1, width = "100px")
          )
        ),
        # only show this panel if the American unit is used
        conditionalPanel(
          condition = "input.sizeUnit == 'American'",
          column(1.6, style='padding:0px;',
            numericInput(inputId = "height1", label = "Height (ft)", value = floor(initial$height/0.3048), min = 0, max = 10, width = "100px")
          ),
          column(1.6, offset = 0, style='padding:0px;',
            numericInput(inputId = "height2", label = "(in)", value = round(12 * (initial$height/0.3048 - floor(initial$height/0.3048)), digits = 1), min = 0, max = 20, width = "100px")
          )
        )
      ),
      # only show this panel if the Metric unit is used
      conditionalPanel(
        condition = "input.sizeUnit == 'metric'",
        column(4, style='padding:0.0px;',
               numericInput(inputId = "weightkg", label = "Weight (kg)", value = initial$weight, min = 0, max = 800, width = "100px")
        )
      ),
      # only show this panel if the American unit is used
      conditionalPanel(
        condition = "input.sizeUnit == 'American'",
        column(4, style='padding:0.0px;',
               numericInput(inputId = "weightlb", label = "Weight (lb)", value = round(initial$weight/0.453592, digits = 1), min = 0, max = 800, width = "100px")
        )
      )
    ),
    fluidRow(
      column(4, style='padding:0px;',
        radioButtons(inputId = "tempUnit", label = "Temperature Unit", choices = c( "\u00B0F" = "FD", "\u00B0C" = "CD"), selected = initial$tempUnit, width = "400px", inline = FALSE)
      ),
      # only show this if FD is used
      column(7, style='padding:0px;',
        conditionalPanel(
          condition = "input.tempUnit == 'FD'",
          column(1.6, style='padding:0px;',
                 numericInput(inputId = "tempFD", label = "Temperature (\u00B0F)", value = round(initial$temp * (9/5) + 32, digits = 1), min = 0, max = 800, width = "400px")
          )
        ),
        # only show this if CD is used
        conditionalPanel(
          condition = "input.tempUnit == 'CD'",
          column(1.6, style='padding:0px;',
                 numericInput(inputId = "tempCD", label = "Temperature (\u00B0C)", value = round(initial$temp, digits = 2), min = 0, max = 800, width = "400px")
          )
        )
      )
    ),
    fluidRow(
      column(11, style='padding:0px;',
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
    time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) * 60 ) # in second
    currentTime = reactive((client_time() - time_zone_offset()) %% 86400)
    currentTimeIndex = round(((currentTime()/60 + 17 * 60)/15) %% 96) + 1
    if(currentTimeIndex > 68){currentTimeIndex = 1
    } else if(currentTimeIndex > 45){currentTimeIndex = 45}

    # check inputs
    validate(need(input$gender != "", "Please input your gender"))
    validate(need(input$age != "", "Please input your age"))
    validate(need(input$age >= rangeData$age[1], "Please input a proper age"))
    validate(need(input$age <= rangeData$age[2], "Please input a proper age"))
    if(input$sizeUnit == "American"){
      validate(need(input$height1 != "", "Please input your height"))
      if(!is.na(input$height1)){
        validate(need(input$height2 != "", "Please input your height"))
      }
      validate(need(input$weightlb != "", "Please input your weight"))
      updateNumericInput(session, "height", label = "Height (m)", value = format(round((input$height1 + input$height2/12)*0.3048, digits = 2)))
      updateNumericInput(session, "weightkg", label = "Weight (kg)", value = format(round(input$weightlb*0.453592, digits = 1)))
    } else if(input$sizeUnit == "metric"){
      validate(need(input$height != "", "Please input your height"))
      validate(need(input$weightkg != "", "Please input your weight"))

      updateNumericInput(session, "height1", label = "Height (ft)", value = format(floor(input$height/0.3048)))
      updateNumericInput(session, "height2", label = "(in)", value = format(round(12 * (input$height/0.3048 - floor(input$height/0.3048)), digits = 1)))
      updateNumericInput(session, "weightlb", label = "Weight (lb)", value = format(round(input$weightkg/0.453592, digits = 1)))
    }

    if(input$sizeUnit == "American"){
      weight.transform = round(input$weightlb * 0.453592)
      height.transform = round((input$height1 + input$height2/12) * 0.3048, digits = 1)
    } else if(input$sizeUnit == "metric"){
      weight.transform = round(input$weightkg)
      height.transform = round(input$height, digits = 1)
    }
    # range
    validate(need(height.transform >= rangeData$height[1], "Please input a proper height"))
    validate(need(height.transform <= rangeData$height[2], "Please input a proper height"))
    validate(need(weight.transform >= rangeData$weight[1], "Please input a proper weight"))
    validate(need(weight.transform <= rangeData$weight[2], "Please input a proper weight"))

    # temperature
    if(input$tempUnit == "CD"){
      updateNumericInput(session, "tempFD", label = "Temperature (\u00B0F)", value = format(round(input$tempCD * (9/5) + 32, digits = 1)))
      temp = input$tempCD
    } else if(input$tempUnit == "FD"){
      updateNumericInput(session, "tempCD", label = "Temperature (\u00B0C)", value = format(round((input$tempFD -32) / (9/5), digits = 2)))
      temp = input$tempFD
    }

    # gender
    if(input$gender == "male"){
      gender.plus = "male"
    } else if(input$age < 40){
      gender.plus = "female.pre"
    } else {
      gender.plus = "female.post"
    }

    # plot
    percentileLevel = c(1,5,25,50,75,95,99)
    preds = baseline[seq(1,45)*15-14,percentileLevel] + gender[rep(gender.plus,45),percentileLevel] + age[rep(as.character(input$age),45), percentileLevel] + height[rep(as.character(height.transform),45),percentileLevel] + weight[rep(as.character(weight.transform),45),percentileLevel]
    if(input$tempUnit == "FD"){
      preds = round(preds, digits=2)
    } else if(input$tempUnit == "CD"){
      preds = round((preds - 32)*5/9, digits=2)
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
    if(!is.na(temp)){
      if(temp > plotData$quantile99[currentTimeIndex]){
        currPercentile = ">99%"
      } else if(temp > plotData$quantile95[currentTimeIndex]){
        currPercentile = paste(round((95 * (plotData$quantile99[currentTimeIndex] - temp)+ 99 * (temp - plotData$quantile95[currentTimeIndex]))/(plotData$quantile99[currentTimeIndex] - plotData$quantile95[currentTimeIndex])), "%", sep = "")
      } else if(temp > plotData$quantile75[currentTimeIndex]){
        currPercentile = paste(round((75 * (plotData$quantile95[currentTimeIndex] - temp)+ 95 * (temp - plotData$quantile75[currentTimeIndex]))/(plotData$quantile95[currentTimeIndex] - plotData$quantile75[currentTimeIndex])), "%", sep = "")
      } else if(temp > plotData$quantile50[currentTimeIndex]){
        currPercentile = paste(round((50 * (plotData$quantile75[currentTimeIndex] - temp)+ 75 * (temp - plotData$quantile50[currentTimeIndex]))/(plotData$quantile75[currentTimeIndex] - plotData$quantile50[currentTimeIndex])), "%", sep = "")
      } else if(temp > plotData$quantile25[currentTimeIndex]){
        currPercentile = paste(round((25 * (plotData$quantile50[currentTimeIndex] - temp)+ 50 * (temp - plotData$quantile25[currentTimeIndex]))/(plotData$quantile50[currentTimeIndex] - plotData$quantile25[currentTimeIndex])), "%", sep = "")
      } else if(temp > plotData$quantile5[currentTimeIndex]){
        currPercentile = paste(round((5 * (plotData$quantile25[currentTimeIndex] - temp)+ 25 * (temp - plotData$quantile5[currentTimeIndex]))/(plotData$quantile25[currentTimeIndex] - plotData$quantile5[currentTimeIndex])), "%", sep = "")
      } else if(temp > plotData$quantile1[currentTimeIndex]){
        currPercentile = paste(round((1 * (plotData$quantile5[currentTimeIndex] - temp)+ 5 * (temp - plotData$quantile1[currentTimeIndex]))/(plotData$quantile5[currentTimeIndex] - plotData$quantile1[currentTimeIndex])), "%", sep = "")
      } else{currPercentile = "<1%"}
      plotData$currentTemp = NA; plotData$currentTemp[currentTimeIndex] = temp
      x = list(title = list(text = "<b> Time <b>", standoff = 20), titlefont = "f", autotick = FALSE, dtick = 8, tickformat="%H:%M:%OS3", tickangle = 0, range = c(-0.5,44.5))
      if(input$tempUnit == "FD"){
        y = list(title = list(text = "<b> Temperature (\u00B0F) <b>", standoff = 30), titlefont = "f", range = c(95.8,99.9), nsmall=2, dtick = 0.4, tickformat = ".1f");
        title = list(text = paste("<b>", "Temperature: ", format(round(temp, digits = 1)), "\u00B0F; ", "Percentile: ", format(currPercentile, nsmall = 0), "<b>", sep = ""))
      } else if(input$tempUnit == "CD"){
        y = list(title = list(text = "<b> Temperature (\u00B0C) <b>", standoff = 30), titlefont = "f", range = c(35.44444,37.72222), dtick = 0.2,  tickformat = ".1f")
        title = list(text = paste("<b>", "Temperature: ", format(round(temp, digits = 1)), "\u00B0C; ", "Percentile: ", format(currPercentile, nsmall = 0), "<b>", sep = ""))
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

