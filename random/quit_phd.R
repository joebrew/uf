

# Function to decide
decide <- function(time_value = 25,
                   credit_value = 500,
                   hours_per_week = 15){
  # Insurance cost = 367 / month (cobra) * 4.5 months
  # that would be covered for free without quitting
  # (apr, may, jun, jul, aug16)
  insurance_cost <- 367 * 4.5 
  # Opportunity_cost are wages I would make between 
  #now and may 11 (7 weeks * 375 / week)
  opportunity_cost <- 7 * 375
  total_cost <- insurance_cost + opportunity_cost + credit_value
  # Hours required
  # work_weeks = how many weeks I'll have to go 
  # to class and such
  work_weeks <- 6
  hours <- work_weeks * hours_per_week
  # Hourly rate
  decision <- (total_cost / hours) > time_value
  # Output
  if(decision){
    obj <- 'Tough it out'
  } else {
    obj <- 'Quit now'
  }
  # Return
  return(obj)
}

time_value <- 1:100
credit_value <- seq(0, 10000, 100)

df <- expand.grid(time_value = time_value,
                  credit_value = credit_value)
df$decision <- NA
for (i in 1:nrow(df)){
  df$decision[i] <- decide(time_value = df$time_value[i],
                           credit_value = df$credit_value[i])
  print(paste0(i, ' of ', nrow(df)))
}

df$col <- ifelse(df$decision == 'Tough it out', 'blue', 'darkgreen')
plot(df$time_value, df$credit_value, col = df$col, pch = 16,
     xlab = 'How much do I value my time (per hour)?',
     ylab = 'How much do I value credit for this semester?')
legend('topleft',
       fill = c('blue', 'darkgreen'),
       legend = c('Tough it out', 'Quit now!'))
title(main = 'Should I quit the PhD now instead of at the end of the semester?')

