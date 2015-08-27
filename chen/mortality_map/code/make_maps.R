#####
# PACKAGES
#####
library(rgdal)
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)

#####
# DIRECTORIES
#####
#wd <- getwd() # Be in the mortality_map/code directory -- change only if needed

#####
# LOAD CHEN'S DATA
#####
df <- read_excel('../data/dataofcountries.xls')

#####
# LOAD IN THE WORLD SHAPEFILE
#####
world <- readOGR('../data/countries_shape_file/', 'world_borders')

#####
# EXPLORE THE SHAPEFILE
#####

# Examine the format
head(world@data)

# Look at distribution of one variable
hist(world@data$POP_CNTRY, breaks = 30, col = 'grey', border = 'white',
     xlab = 'Population', main = 'Distribution of countries\' population')

# Make a simple map
plot(world, col = 'grey', border = 'white')

# Make a new variable in world: population density
# this will be each countries' population divided by its area
world@data$POP_DEN <- world@data$POP_CNTRY / world@data$AREA

# Examine the distribution of population densities (this time using ggplot instead of base graphics)
ggplot(data = world@data, aes(x = POP_DEN)) +
  geom_density(fill = 'blue', alpha = 0.5) +
  theme_bw() +
  xlab('People per square kilometer') +
  ggtitle('Distribution of population densities') +
  scale_x_log10() # put on log 10 scale

# Make a choropleth in which each country is shaded by the square root of its population density
world@data$POP_DEN_SQRT <- ceiling(sqrt(world@data$POP_DEN))
colors <- colorRampPalette(c('red', 'yellow'))(max(world@data$POP_DEN_SQRT)) %>%
  adjustcolor(alpha.f = 0.9)
plot(world, col = colors, border = NA, main = 'Population density')
legend('bottomleft',
       fill = colors[c(1, length(colors)/2, length(colors))],
       legend = rev(round(c(1, length(colors)/2, length(colors)) ^ 2, digits = -2)),
       title = 'Inhabitants per square kilometer',
       bty = 'n',
       cex = 0.5,
       border = NA)

#####
# MERGE THE MORTALITY AND LIFE EXPECTANCY DATA (df) WITH THE WORLD SHAPEFILE
#####

# We will join on the 'Country' column in df
# and the 'CNTRY_NAME' column in world
# However, since these are unlikely to be identically named
# (and we don't want to take the time to manually change them)
# we'll write a "fuzzy matching" algorithm, which will 
# create a column in df with the 'CNTRY_NAME' from world@data
# which is most similar to that row's 'Country' column

fuzzy_match <- function(x = world@data$CNTRY_NAME,
                        y = df$Country){
  # Create matrix of "string distance" between names
  distances <- adist(x = x, y = y)
  # For each row, get the index of the lowest (best-matching) column
  best_matches <- 
    apply(distances, 1, function(z){
      possibs <- which.min(z)
      return(possibs[1])
    })

  # Return the actual names of the best matches
  names <- y[best_matches]
  distance <- apply(distances, 1, min)
  return_object <- list('name' = names, 'distance' =  distance)
  return(return_object)
}

# Now, use our algorithm to populate a new column in "world"
# with the names from "df" (so that we can correctly merge)
results <- fuzzy_match()
world@data$Country <- results$name

# We can also populate a column which shows us the "string distance"
# (ie, how closely matched the terms were)
# high numbers are bad, low numbers are good.  0 is a perfect match
world@data$accuracy <- results$distance

# Now let's take another look at our world shapefile
head(world@data)

# And examine the accuracy results
hist(world@data[!duplicated(world@data$FIPS_CNTRY),]$accuracy, breaks = 30,
     xlab = 'Character transformation',
     main = 'Accuracy (0 = perfect)',
     col = 'grey',
     border = 'white')

# So, our algorithm was pretty good, but there are sill a few countries with 
# pretty bad results.  For example, let's just look at those
# countries who took more than 10 character transformations
world@data[which(world@data$accuracy >= 10 & !duplicated(world@data$FIPS_CNTRY)),
           c('CNTRY_NAME', 'Country')]

# Clearly, the above isn't good!

# However, the algorithm did pretty good on most
world@data[which(world@data$accuracy <=2 & !duplicated(world@data$FIPS_CNTRY)),
           c('CNTRY_NAME', 'Country')][1:10,]

# For now, we'll use the algorithm.  If you want a perfect match,
# you'll need to go through your spreadsheet (dataofcountries.xls)
# and rename "Country" so that each name is one of the 
# elements of the "correct_countries" vector
correct_countries <- unique(sort(world@data$CNTRY_NAME))

# (I've printed the entire vector at the end of this document)
# and then re-run the code

#####
# MERGE
#####
# Anyway, now that MOST of the data is matched, we'll merge
world@data <- left_join(x = world@data, y = df)

# Now we can see that we've brough the data in from df into world@data
head(world@data)

# Since the algorithm wasn't able to match ALL the countries,
# we're only going to keep those perfect accuracy
world_accurate <- world[which(world@data$accuracy == 0),]

# Let's see which countries we were able to keep
plot(world)
plot(world_accurate, col = 'red', add = TRUE)
legend('left',
       fill = c('white', 'red'),
       legend = c('Inaccurate match', 'Accurate match'),
       cex = 0.7)

#####
# PLOT
#####

# Let's define a simple function for plotting a choropleth
choro <- function(shape = world_accurate,
                  background = world,
                  colors = c('red', 'yellow', 'blue'),
                  variable = 'MR2000_100000population'){
  variables <- c('MR2000_100000population', 'MR2012_100000population',
                 'LE2000_year', 'LE2012_year')
  if(!variable %in% variables){
    stop(paste0('variable must be one of ', paste0(variables, collapse = ', ')))
  }
  plot(background, col = 'grey', border = NA)
  
  # Get title information
  tit <- paste0(
    ifelse(substr(variable, 1,2) == 'MR', 'Morality', 'Life expectancy'),
    ' in ',
    substr(variable, 3,6)
  )
  
  # Create a var for plotting
  var <- ceiling(shape@data[,variable])
  cols <- colorRampPalette(colors)(max(var, na.rm = TRUE))
  # (reverse color scheme if mortality)
  if(substr(variable, 1, 2) == 'MR'){
    cols <- rev(cols)
  }
  var_cols <- cols[var]
  
  # Plot
  plot(shape, col = var_cols, add = TRUE, border = NA)
  title(main = tit)
  
  # Legend
  legend_vec <- 1:length(cols)
  legend_quant <- round(as.numeric(quantile(legend_vec, na.rm = TRUE)))
  legend('left',
         fill = c(cols[legend_quant], 'grey'),
         legend = c(legend_quant, 'No data'),
         cex = 0.7,
         border = NA)
}



# Now let's plot all of our variables
variables <- c('MR2000_100000population', 'MR2012_100000population',
               'LE2000_year', 'LE2012_year')

choro(variable = variables[1])
choro(variable = variables[2])
choro(variable = variables[3])
choro(variable = variables[4])

# Alternatively, we could plot all the countries (even those we know weren't correctly matched)
choro(variable = variables[1], shape = world)
choro(variable = variables[2], shape = world)
choro(variable = variables[3], shape = world)
choro(variable = variables[4], shape = world)


#####
# NEXT STEPS
#####

# In order to get the matches perfect,
# go through the spreadsheet with your data
# and rename all the countries to one of the following:
print(correct_countries)

