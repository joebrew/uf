df <- data.frame(bowl = c(rep(1, 20), rep(2, 40)),
                 col = c(rep('red', 10), rep('blue', 10),
                         rep('red', 20), rep('blue', 20)))

picked_blue <- df[which(df$col == "blue"),]

tab <- table(picked_blue$bowl)
ptab <- prop.table(tab)
barplot(ptab, names.arg = paste("bowl", names(ptab)))
