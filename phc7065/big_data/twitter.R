# Good walkthrough at: http://geoffjentry.hexdump.org/twitteR.pdf

#####
# INSTALL AND ATTACH THE twitteR PACKAGE
#####
# install.packages('twitteR')
library(twitteR)

#####
# SET UP AUTHENTICATION PROTOCOLS
#####
setup_twitter_oauth("BSvxWsWnMirL7RmRTZM9JsWT3", 
                    "J7TxKqSosyBMbuJO5K3ZlYLuOHWo3zTQj5ljjMZ5vzMhXv8ILn", 
                    "492031406-nBhrqZd1llYc2WUEJtDkchxmfYXhkVqrmze3QWZr", 
                    "5HeJxWN48jLEimrC4PTmUYiCK5e493b1OFr3SHHsIeFc9")
2
#####
# SEARCH TWITTER FOR A TERM OF INTEREST
#####
tweets <- searchTwitter(searchString = 'dog', 
                        n= 500,
                        geocode='29.6520,-82.3250,50mi',
                        #since = '2014-10-01'
                        )

# Get rid of retweets
tweets <- strip_retweets(tweets)

#####
# CONVERT TO DATAFRAME
#####
df <- twListToDF(tweets)

df$text

# Remove non -ascii
x <- iconv(df$text, sub = '')
df <- df[which(df$text == x),]

# Get numeric lon lat
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)

library(wordcloud)

# Clean up words
remove <- "[|]|[(]|[])]|/|;|:|[(*]|&|-[)]|[(]|[.]|[-]|[--])"
temp <- unlist(strsplit(as.character(gsub(remove,"",
                                             (df$text))), " "))

# Use only one
temp <- unique(sort(temp))

# Get non ASCII
bads <- grep("temp", iconv(temp, "latin1", "ASCII", sub="temp"))
temp <- temp[-bads]

# Make dataframe and get counts
temp <- data.frame(word = temp,
                   n = NA)
temp$word <- as.character(temp$word)

for (i in 1:nrow(temp)){
  temp$n[i] <- 
    nrow(df[which(grepl(temp$word[i], df$text)),])
}
# Order
temp <- temp[rev(order(temp$n)),]
head(temp, n = 20)

cols <- rev(colorRampPalette(c('grey', 'black'))(nrow(temp)))
wordcloud(words = temp$word,
          freq = temp$n,
          random.order = FALSE,
          scale = c(2, 0.5),
          colors = cols,
          ordered.colors = TRUE,
          min.freq = 1, rot.per = 0.7)
bp <- barplot(temp$n[1:20],
        col = 'darkorange', border = FALSE, yaxt = "n")
text(bp[,1], y = 0, 
     pos = 3,
     labels = temp$word[1:20],
     las = 1)

#####
# MAP
#####
library(maps)
map('county', 'fl')
points(df$longitude, df$latitude, col = 'red')

map('county', 'fl')
text(df$longitude, df$latitude,
     col = adjustcolor('black', alpha.f = 0.6),
     labels = df$text,
     cex = 0.2
     )


#####
# TWITTER-MAINTAINED TRENDS
#####

# List of all available locations
avail_trends <- availableTrendLocations()
# Closest location to GNV
close_trends <- closestTrendLocations(29.6520,-82.3250)

# Get trends for closest location
trends <- getTrends(as.numeric(close_trends$woeid))


#####
# ONE MORE TERM
#####
#####
# SEARCH TWITTER FOR A TERM OF INTEREST
#####
tweets <- searchTwitter(searchString = 'ebola', 
                        n= 1000,
                        #since = '2014-10-01'
)

# Get rid of retweets
tweets <- strip_retweets(tweets)

#####
# CONVERT TO DATAFRAME
#####
df <- twListToDF(tweets)

df$text

# Remove non -ascii
x <- iconv(df$text, sub = '')
df <- df[which(df$text == x),]

# Get numeric lon lat
df$longitude <- as.numeric(df$longitude)
df$latitude <- as.numeric(df$latitude)


#####
# MAP
#####
map('world', fill = TRUE, col = 'grey')
points(df$longitude, df$latitude,
     col = adjustcolor('red', alpha.f = 0.6),
     cex = 2,
     pch = 16
)

#####
#
#####

#####
#
#####

#####
#
#####

#####
#
#####
#####
#
#####

#####
#
#####

#####
#
#####

#####
#
#####

#####
#
#####
#####
#
#####

#####
#
#####

#####
#
#####

#####
#
#####

#####
#
#####