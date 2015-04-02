library(ggmap)
library(maps)

address <- c(
  'Big Ben, London',
  'Maude\'s, Gainesville, Florida',
  'Facebook headquarters, Menlo Park, California',
  'CIA headquarters',
  'Tierra de Fuego, Argentina',
  'Djakarta, Indonesia',
  'Mekelle, Ethiopia')

temp <- geocode(address)

# Merge back together
df <- cbind(address, temp)

# Map the world
map('world')

# Add points
points(x = df$lon,
       y = df$lat,
       col = 'red',
       pch = 16,
       cex = 2)

# Add text
map('world')
text(x = df$lon,
     y = df$lat,
     col = 'darkblue',
     labels = df$address
     )
