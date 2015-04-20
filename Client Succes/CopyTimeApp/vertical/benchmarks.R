setwd("/Users/brentducote/strategicinsights/Benchmarks")
AD <- read.csv("All_Data_04.06.csv", header=TRUE)
AD <- AD[AD$vertical == "Sports",]
#AD <- AD[AD$client == "YP",]

#### variables ####

imps <- sum(AD$impressions, na.rm = TRUE)
linkImps <- sum(AD$impressions[AD$link == 1], na.rm = TRUE)
clicks <- sum(AD$url.clicks, na.rm = TRUE)
favs <- sum(AD$favorites, na.rm = TRUE)
RTs <- sum(AD$retweets, na.rm = TRUE)
replies <- sum(AD$replies, na.rm = TRUE)
followBacks <- length(AD$followed_back_at) - length(AD$followed_back_at[AD$followed_back_at == "NA"])

# click rate
CTR <- clicks/linkImps
clickRate <- clicks/length(AD$text[AD$link ==1])


# acquisition rate
acqRate <- followBacks / (nrow(AD[is.na(AD$followed_at) == FALSE,]))

# expanded imps (RT rate)

RTrate <- RTs/imps
RTconRate <- RTs / length(AD$retweets)

# replies
replyRate <- replies/imps 
replyconRate <- replies/length(AD$replies)

# favorites
favRate <- favs/imps
favconRate <- favs/length(AD$favorites)

benchmarkMatrix <- matrix(c(CTR*100,clickRate*100,acqRate*100,"NA",RTrate*100,RTconRate*100,replyRate*100,replyconRate*100,favRate*100,favconRate*100),ncol = 5, byrow= FALSE)
colnames(benchmarkMatrix) <- c("clicks","acquisition","RTs","replies","favs")
rownames(benchmarkMatrix) <- c("impression based", "tweet count based")

View(benchmarkMatrix)

write.csv(benchmarkMatrix, "Sports Benchmarks.csv")