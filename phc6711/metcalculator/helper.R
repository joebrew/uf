
# Read in activities data
activities <- read.csv("activities.csv")

# Clean up activities data
activities <- activities[c(1,2,3,4,6,8,
                          10, 11, 14, 15, 16, 17,19,
                          20, 21, 22, 25, 27, 28,
                          29, 30, 32, 33, 34, 35, 37, 39,
                          41, 42, 45, 48),]

# Create empty columns
activities$number_of_times <- NA
activities$avg_duration <- NA
activities$met <- NA
activities$calculated_met <- NA

# Clean up spaces
activities$Activity <- gsub(" |-", "_", activities$Activity)

## Prints out everything I need for the really long ui.R
## Had to eliminate a few, since so many inputs were causing json errors in the app
# paste_text <- function(activity){
#   cat(paste0("textInput(inputId = '", activity,"',
#             label = '", activity, "',
#             value = '0'),\n
#   checkboxInput('", activity,"_vig', label = 'Vigorous', value = FALSE),
#   sliderInput('", activity,"_dur', label = 'Duration (in hours)', 
#               min=0, max=5, value=0, step=0.25),
# 
# br(),
#              "))
# }
# 
# for (i in 1:nrow(activities)){
#   paste_text(activities$Activity[i])
# }

#### CODE TO GET EACH OF THE ACTIVITIES TO THEIR CORRESPONDING PLACES

# 
# paste_text <- function(activity){
#   cat(paste0("
# 
#              activities2$number_of_times[which(activities2$Activity == '", activity,"')] <-
#                as.numeric(input[['", activity, "']]) 
#              
#              \nactivities2$avg_duration[which(activities2$Activity == '", activity,"')] <-
#                as.numeric(input[['", paste0(activity,"_dur"), "']])
#              
#              
#              \nactivities2$met[which(activities2$Activity == '", activity,"')] <-
#                as.numeric(ifelse(input[['", paste0(activity, "_vig"), "']],",
#              "activities2$vigorous[which(activities2$Activity == '", activity,"')],
#                       activities2$moderate[which(activities2$Activity == '", activity,"')]))
#     
#              "))
# }
# 
# for (i in activities$Activity){
#   paste_text(i)
# }



# activities$number_of_times <- NA
# for (i in 1:nrow(activities)){
#   
#   activity <- activities$Activity[i]
#   
#   
#   
#   activities$number_of_times[i] <- 
#     print(input[activity])
#     
# }

# Function for plotting
bar_fun <- function(data = activities3, var = "calculated_met"){

  my_cols <- colorRampPalette(c("blue", "lightblue", "white", "yellow", "orange", "red"))(nrow(data))
  my_cols <- adjustcolor(my_cols, alpha.f = 0.4)
  data <- data[rev(order(data[,var])),]
  vec <- data[,var]
  vec_names <- data[,"Activity"]
      bp <- barplot(vec, names.arg = vec_names, las = 3,
                    ylim = c(0, max(vec, na.rm = TRUE) * 1.1),
                    cex.names = 0.75,
                    col = my_cols,
                    border = NA)
      text(x = bp[,1],
           y = vec,
           labels = round(vec),
           pos = 3)
  
  text(x = bp[27,1],
       y = max(vec, na.rm = T) * 0.8,
       labels = paste0("Total monthly MET's: ", sum(vec, na.rm = T)))
      box("plot")
  title(main = "Distribution of your monthly metabolic equivalent tasks")
}

