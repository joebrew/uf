
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# Source the helper functions
source("helper.R")

shinyServer(function(input, output) {
  
  person_data <- reactive({
    x <- data.frame(height = as.numeric(input[['height']]),
               weight = as.numeric(input[['weight']]),
               bmi = as.numeric(NA))
    
    x$bmi <- (as.numeric(input$weight) * 703) /
      (as.numeric(input$height)^2)
    
    x$double_bmi <- x$bmi * 2
    
    kg <- 0.453592 * x$weight
    
    #x$calories_per_met <- 
    
    x

    })
  
  activities3 <- reactive({
    activities2 <- data.frame(activities)


 
    activities2$number_of_times[which(activities2$Activity == 'Aerobics')] <-
      as.numeric(input[['Aerobics']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Aerobics')] <-
      as.numeric(input[['Aerobics_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Aerobics')] <-
      as.numeric(ifelse(input[['Aerobics_vig']],activities2$vigorous[which(activities2$Activity == 'Aerobics')],
                        activities2$moderate[which(activities2$Activity == 'Aerobics')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Baseball')] <-
      as.numeric(input[['Baseball']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Baseball')] <-
      as.numeric(input[['Baseball_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Baseball')] <-
      as.numeric(ifelse(input[['Baseball_vig']],activities2$vigorous[which(activities2$Activity == 'Baseball')],
                        activities2$moderate[which(activities2$Activity == 'Baseball')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Basketball')] <-
      as.numeric(input[['Basketball']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Basketball')] <-
      as.numeric(input[['Basketball_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Basketball')] <-
      as.numeric(ifelse(input[['Basketball_vig']],activities2$vigorous[which(activities2$Activity == 'Basketball')],
                        activities2$moderate[which(activities2$Activity == 'Basketball')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Bicycling')] <-
      as.numeric(input[['Bicycling']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Bicycling')] <-
      as.numeric(input[['Bicycling_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Bicycling')] <-
      as.numeric(ifelse(input[['Bicycling_vig']],activities2$vigorous[which(activities2$Activity == 'Bicycling')],
                        activities2$moderate[which(activities2$Activity == 'Bicycling')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Dance')] <-
      as.numeric(input[['Dance']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Dance')] <-
      as.numeric(input[['Dance_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Dance')] <-
      as.numeric(ifelse(input[['Dance_vig']],activities2$vigorous[which(activities2$Activity == 'Dance')],
                        activities2$moderate[which(activities2$Activity == 'Dance')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Football')] <-
      as.numeric(input[['Football']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Football')] <-
      as.numeric(input[['Football_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Football')] <-
      as.numeric(ifelse(input[['Football_vig']],activities2$vigorous[which(activities2$Activity == 'Football')],
                        activities2$moderate[which(activities2$Activity == 'Football')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Golf')] <-
      as.numeric(input[['Golf']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Golf')] <-
      as.numeric(input[['Golf_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Golf')] <-
      as.numeric(ifelse(input[['Golf_vig']],activities2$vigorous[which(activities2$Activity == 'Golf')],
                        activities2$moderate[which(activities2$Activity == 'Golf')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Hiking')] <-
      as.numeric(input[['Hiking']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Hiking')] <-
      as.numeric(input[['Hiking_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Hiking')] <-
      as.numeric(ifelse(input[['Hiking_vig']],activities2$vigorous[which(activities2$Activity == 'Hiking')],
                        activities2$moderate[which(activities2$Activity == 'Hiking')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Jogging')] <-
      as.numeric(input[['Jogging']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Jogging')] <-
      as.numeric(input[['Jogging_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Jogging')] <-
      as.numeric(ifelse(input[['Jogging_vig']],activities2$vigorous[which(activities2$Activity == 'Jogging')],
                        activities2$moderate[which(activities2$Activity == 'Jogging')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Kayaking')] <-
      as.numeric(input[['Kayaking']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Kayaking')] <-
      as.numeric(input[['Kayaking_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Kayaking')] <-
      as.numeric(ifelse(input[['Kayaking_vig']],activities2$vigorous[which(activities2$Activity == 'Kayaking')],
                        activities2$moderate[which(activities2$Activity == 'Kayaking')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Push_ups')] <-
      as.numeric(input[['Push_ups']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Push_ups')] <-
      as.numeric(input[['Push_ups_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Push_ups')] <-
      as.numeric(ifelse(input[['Push_ups_vig']],activities2$vigorous[which(activities2$Activity == 'Push_ups')],
                        activities2$moderate[which(activities2$Activity == 'Push_ups')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Racquetball')] <-
      as.numeric(input[['Racquetball']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Racquetball')] <-
      as.numeric(input[['Racquetball_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Racquetball')] <-
      as.numeric(ifelse(input[['Racquetball_vig']],activities2$vigorous[which(activities2$Activity == 'Racquetball')],
                        activities2$moderate[which(activities2$Activity == 'Racquetball')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Rowing')] <-
      as.numeric(input[['Rowing']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Rowing')] <-
      as.numeric(input[['Rowing_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Rowing')] <-
      as.numeric(ifelse(input[['Rowing_vig']],activities2$vigorous[which(activities2$Activity == 'Rowing')],
                        activities2$moderate[which(activities2$Activity == 'Rowing')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Running')] <-
      as.numeric(input[['Running']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Running')] <-
      as.numeric(input[['Running_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Running')] <-
      as.numeric(ifelse(input[['Running_vig']],activities2$vigorous[which(activities2$Activity == 'Running')],
                        activities2$moderate[which(activities2$Activity == 'Running')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Sit_ups')] <-
      as.numeric(input[['Sit_ups']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Sit_ups')] <-
      as.numeric(input[['Sit_ups_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Sit_ups')] <-
      as.numeric(ifelse(input[['Sit_ups_vig']],activities2$vigorous[which(activities2$Activity == 'Sit_ups')],
                        activities2$moderate[which(activities2$Activity == 'Sit_ups')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Skating')] <-
      as.numeric(input[['Skating']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Skating')] <-
      as.numeric(input[['Skating_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Skating')] <-
      as.numeric(ifelse(input[['Skating_vig']],activities2$vigorous[which(activities2$Activity == 'Skating')],
                        activities2$moderate[which(activities2$Activity == 'Skating')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Soccer')] <-
      as.numeric(input[['Soccer']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Soccer')] <-
      as.numeric(input[['Soccer_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Soccer')] <-
      as.numeric(ifelse(input[['Soccer_vig']],activities2$vigorous[which(activities2$Activity == 'Soccer')],
                        activities2$moderate[which(activities2$Activity == 'Soccer')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Stair_Climbing')] <-
      as.numeric(input[['Stair_Climbing']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Stair_Climbing')] <-
      as.numeric(input[['Stair_Climbing_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Stair_Climbing')] <-
      as.numeric(ifelse(input[['Stair_Climbing_vig']],activities2$vigorous[which(activities2$Activity == 'Stair_Climbing')],
                        activities2$moderate[which(activities2$Activity == 'Stair_Climbing')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Stretching')] <-
      as.numeric(input[['Stretching']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Stretching')] <-
      as.numeric(input[['Stretching_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Stretching')] <-
      as.numeric(ifelse(input[['Stretching_vig']],activities2$vigorous[which(activities2$Activity == 'Stretching')],
                        activities2$moderate[which(activities2$Activity == 'Stretching')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Swimming')] <-
      as.numeric(input[['Swimming']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Swimming')] <-
      as.numeric(input[['Swimming_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Swimming')] <-
      as.numeric(ifelse(input[['Swimming_vig']],activities2$vigorous[which(activities2$Activity == 'Swimming')],
                        activities2$moderate[which(activities2$Activity == 'Swimming')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Tennis')] <-
      as.numeric(input[['Tennis']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Tennis')] <-
      as.numeric(input[['Tennis_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Tennis')] <-
      as.numeric(ifelse(input[['Tennis_vig']],activities2$vigorous[which(activities2$Activity == 'Tennis')],
                        activities2$moderate[which(activities2$Activity == 'Tennis')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Volleyball')] <-
      as.numeric(input[['Volleyball']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Volleyball')] <-
      as.numeric(input[['Volleyball_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Volleyball')] <-
      as.numeric(ifelse(input[['Volleyball_vig']],activities2$vigorous[which(activities2$Activity == 'Volleyball')],
                        activities2$moderate[which(activities2$Activity == 'Volleyball')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Walking')] <-
      as.numeric(input[['Walking']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Walking')] <-
      as.numeric(input[['Walking_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Walking')] <-
      as.numeric(ifelse(input[['Walking_vig']],activities2$vigorous[which(activities2$Activity == 'Walking')],
                        activities2$moderate[which(activities2$Activity == 'Walking')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Weight_Lifting')] <-
      as.numeric(input[['Weight_Lifting']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Weight_Lifting')] <-
      as.numeric(input[['Weight_Lifting_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Weight_Lifting')] <-
      as.numeric(ifelse(input[['Weight_Lifting_vig']],activities2$vigorous[which(activities2$Activity == 'Weight_Lifting')],
                        activities2$moderate[which(activities2$Activity == 'Weight_Lifting')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Yard_Work')] <-
      as.numeric(input[['Yard_Work']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Yard_Work')] <-
      as.numeric(input[['Yard_Work_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Yard_Work')] <-
      as.numeric(ifelse(input[['Yard_Work_vig']],activities2$vigorous[which(activities2$Activity == 'Yard_Work')],
                        activities2$moderate[which(activities2$Activity == 'Yard_Work')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Frisbee')] <-
      as.numeric(input[['Frisbee']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Frisbee')] <-
      as.numeric(input[['Frisbee_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Frisbee')] <-
      as.numeric(ifelse(input[['Frisbee_vig']],activities2$vigorous[which(activities2$Activity == 'Frisbee')],
                        activities2$moderate[which(activities2$Activity == 'Frisbee')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Martial_Arts')] <-
      as.numeric(input[['Martial_Arts']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Martial_Arts')] <-
      as.numeric(input[['Martial_Arts_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Martial_Arts')] <-
      as.numeric(ifelse(input[['Martial_Arts_vig']],activities2$vigorous[which(activities2$Activity == 'Martial_Arts')],
                        activities2$moderate[which(activities2$Activity == 'Martial_Arts')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Yoga')] <-
      as.numeric(input[['Yoga']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Yoga')] <-
      as.numeric(input[['Yoga_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Yoga')] <-
      as.numeric(ifelse(input[['Yoga_vig']],activities2$vigorous[which(activities2$Activity == 'Yoga')],
                        activities2$moderate[which(activities2$Activity == 'Yoga')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Gymnastics')] <-
      as.numeric(input[['Gymnastics']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Gymnastics')] <-
      as.numeric(input[['Gymnastics_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Gymnastics')] <-
      as.numeric(ifelse(input[['Gymnastics_vig']],activities2$vigorous[which(activities2$Activity == 'Gymnastics')],
                        activities2$moderate[which(activities2$Activity == 'Gymnastics')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Skateboarding')] <-
      as.numeric(input[['Skateboarding']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Skateboarding')] <-
      as.numeric(input[['Skateboarding_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Skateboarding')] <-
      as.numeric(ifelse(input[['Skateboarding_vig']],activities2$vigorous[which(activities2$Activity == 'Skateboarding')],
                        activities2$moderate[which(activities2$Activity == 'Skateboarding')]))
    
    
    
    activities2$number_of_times[which(activities2$Activity == 'Other')] <-
      as.numeric(input[['Other']]) 
    
    
    activities2$avg_duration[which(activities2$Activity == 'Other')] <-
      as.numeric(input[['Other_dur']])
    
    
    
    activities2$met[which(activities2$Activity == 'Other')] <-
      as.numeric(ifelse(input[['Other_vig']],activities2$vigorous[which(activities2$Activity == 'Other')],
                        activities2$moderate[which(activities2$Activity == 'Other')]))
    
    # CALCULATE MET
    activities2$calculated_met <-
      activities2$number_of_times *
      (activities2$avg_duration/60) *
      activities2$met
    
    # CALCULATE CALORIES BURNED
    activities2$calories_burned <- 
      activities2$calculated_met * #duration * met
      (as.numeric(input[['weight']]) * 0.453592)   # weight
      
    

    activities2 <- activities2[order(rev(activities2$calculated_met)),]
    activities2
    
    
    
  })
  
  
  # CLEAN UP THE ACTIVITIES DATA FRAME
  
  # CALORIE PLOT
  output$calorie_plot <- renderPlot({
    par(mar = c(6, 4, 1, 1))
    par(oma = c(4, 0, 0, 0))
    if(sum(activities3()$calories_burned, na.rm = TRUE) == 0){
      
      plot(1:10, 1:10, col = "white", pch = NA,
           xlab = NA,
           ylab = NA)
      title(main = "Provide your activity information to make a plot here")
      
    } else{
      calorie_plot(data = activities3())
      
    }
    
  })
  
  # MET PLOT
  output$met_plot <- renderPlot({
    # Barplot
    par(mar = c(6, 4, 1, 1))
    par(oma = c(4, 0, 0, 0))

    if(sum(activities3()$calculated_met, na.rm = TRUE) == 0){
      
      plot(1:10, 1:10, col = "white", pch = NA,
           xlab = NA,
           ylab = NA)
      title(main = "Provide your activity information to make a plot here")
      
    } else{
      bar_fun(data = activities3())
      
    }

    
  })
  
  
  output$bmi_table <- renderTable({
    
    data.frame(person_data())

  },
  options = list(
    paging = FALSE))
  
  output$bmi_plot <- renderPlot({
    
    my_hist <- hist(bmi, breaks = 50, freq = FALSE,
         border = NA, col = "grey",
         main = "Your BMI relative to other American adults",
         xlim = c(15, 40))
    draw_bmi <- (as.numeric(input$weight) * 703) /
      (as.numeric(input$height)^2)
    abline(v = draw_bmi,
           col = adjustcolor("blue", alpha.f = 0.7),
           lwd = 2)
    
    text(x = draw_bmi,
         y = max(my_hist$density) * 0.9,
         pos = 2,
         labels = paste0("Your BMI: ", round(draw_bmi, digits = 2)),
         col = adjustcolor("darkblue", alpha.f = 0.6))
    
    abline(v = c(18.5, 25, 30),
           col = adjustcolor("darkred", alpha.f = 0.6))
    
    text(x = c(21.75, 27.5, 33),
         y = max(my_hist$density) * 0.2,
         pos = 3,
         labels = c("Normal", "Overweight", "Obese"),
         col = adjustcolor("black", alpha.f = 0.6))
    
    box("plot")

  })
  
  
  
  
  output$my_table <- renderDataTable({

    #calculate_met("Aerobics")
    data.frame(activities3())
    },
    options = list(
      paging = FALSE))

})
