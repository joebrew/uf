a <- c(1, 120, 2, 5, 12, 30, 12, 12, 30, 4, 4)
b <- c(1, 0.25, 1, 2, 0.12, 0.5, 0.12, 1, 0.5, 0.25, 1.5)
c <- c(8, 8, 4.5, 6, 8, 10, 8, 8, 5, 4, 8)
activities <- c("Basketball", "Bicycling", "Dance", "Hiking",
                "Push-ups", "Running", "Sit-ups", "Swimming",
                "Walking", "Yard work", "Frisbee")

df <- data.frame(activity = activities,
                 number_of_times = a,
                 avg_duration = b,
                 met = c)

df$calculated_met <- a * b * c

# order by calculated_met
df <- df[order(df$calculated_met),]

# Barplot
bp <- barplot(df$calculated_met, names.arg = df$activity, las = 3,
              ylim = c(0, max(df$calculated_met, na.rm = TRUE) * 1.1))
text(x = bp[,1],
     y = df$calculated_met,
     labels = round(df$calculated_met),
     pos = 3)
box("plot")
