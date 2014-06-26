#PROBLEM 1
#1 a
usPop <- 281421906
osteo <- 10140000
osteoNew <- 250000

# period prevalence (per 1000) of osteo
osteo / usPop *1000
#assuming osteo includes the osteoNew
#already

# so not this ((osteo + osteoNew)/usPop)*1000

#1 b
1000*(osteoNew / (usPop-osteo))
#about 1 case per every 1000 people

#PROBLEM 2
n <- 1050
N <- 100000
cases <- 50
deaths <- 40
casesNew <- 100
deathsNew <- 50
deathsNonX <- (n-cases)*0.15

#2.1
#prev at initial exam
50 / (1050)

#2.2
#prev of disease at time of re-exam
(cases-deaths+casesNew-deathsNew)/
  (n-deaths-deathsNew)

#PROBLEM 3
schiz <- 1000
N <- 2000000
incidence <- 5/100000

prev <- schiz/N

casesNew <- N*incidence
casesNew / schiz*100


#PROBLEM 4
#c = 6

#incidence for 1961
# 0 per 100000

# PROBLEM 5
N <- 20000
smokers <- 10000
smokersNon <- 10000
smokersHeart <- 400
smokersNonHeart <- 300

(smokersHeart+smokersNonHeart) / N

# PROBLEM 6
#b

#PROBLEM 7

#Problem 8

act <- as.data.frame(c("0-12",
                       "13-24",
                       "25-36",
                       "37-48",
                       "49-60"))
colnames(act) <- "j"
act$N <- c(3000, 
           3000-110-23, 
           3000-110-23-57-45, 
           3000-110-23-57-45-98-20, 
           3000-110-23-57-45-98-20-25-11)
act$deaths <- c(110, 57, 98, 25, 70)
act$Wj <- c(23,45,20,11,58)
    
act$Nj <- act$N - (act$Wj/2)

act$qj <- act$deaths / act$Nj
act$Pj <- 1-act$qj

act$Sj[1] <- act$Pj[1]
act$Sj[2] <- act$Pj[1]*act$Pj[2]
act$Sj[3] <- act$Sj[2]*act$Pj[3]
act$Sj[4] <- act$Sj[3]*act$Pj[4]
act$Sj[5] <- act$Sj[4]*act$Pj[5]

act$CI <- 1- act$Sj
#0.124 is cumulative prob of survial after 60

#PROBLEM 9

