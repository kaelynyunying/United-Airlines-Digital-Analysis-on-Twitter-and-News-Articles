set.seed(123)
library(plyr)
library(dplyr)
library(readr)

#Q1 How do you identify top publishers and influencers?
#Which publishers and influencers should United Airlines pay attention to and why?

######### Influencers ##########
distinct_twitter_influencers_addedattributes <- read_csv("C:/Users/xinmin/Desktop/NUS/Y2S1/epic/distinct_twitter_influencers_addedattributes.csv")
df <- distinct_twitter_influencers_addedattributes
df1 <- df %>% select("twitter_id","twitter_handle","followers_count", "following_count", "likes_count", "max_retweet_value", "social_referrals", "statuses_count", "socialrefcount","favourite_count_per_post", "rt_count_per_post")
df1 = distinct(df1, twitter_id, .keep_all = TRUE) #only keep unique twitter IDs

wss <- rep(NA, 10)

for(k in c(1:10)) {
  wss[k] = kmeans(df1[,-c(1,2,7)], k, nstart = 10)$tot.withinss
} #To find out the optimal number of clusters to use since high cluster no will make us prone to overfitting

plot(wss, type ="b", xlab = "Number of clusters", "ylab" = "Total within-cluster sum of squares")
#Use 2 clusters as it results in the greatest fall in total within cluster sum of sqs.

cluster_no_vec <- kmeans(df1[,-c(1,2,7)], 2, nstart=10)
df1$Cluster_No <- cluster_no_vec[[1]] #Adding the cluster number back to the original dataframe
table(df1$Cluster_No)

Group1 <- df1[df1$Cluster_No == 1,]  #8327 records
Group2 <- df1[df1$Cluster_No == 2,]  #20 records

#Further split Group1, since having 2 clusters only will give us limited information about the different clusters
#Try to determine clusters within group1
wss1 <- rep(NA, 10)

for(k in c(1:10)) {
  wss1[k] = kmeans(Group1[,-c(1,2,7,12)], k, nstart = 10)$tot.withinss
} #To find out the optimal number of clusters to use since high cluster no will make us prone to overfitting

plot(wss1, type ="b", xlab = "Number of clusters", "ylab" = "Total within-cluster sum of squares")
#Use 2 clusters as it results in the greatest fall in total within cluster sum of sqs.

cluster_no_vec <- kmeans(Group1[,-c(1,2,7,12)], 2, nstart=10)
Group1$Cluster_No2 <- cluster_no_vec[[1]] #Adding the cluster number back to the original dataframe

Group1.1 <- Group1[Group1$Cluster_No2 == 1,]  #8243 records 
Group1.2 <- Group1[Group1$Cluster_No2 == 2,]  #84 records 

#Further split Group1.1 
#try to determine clusters within group1.1
wss2 <- rep(NA, 10)

for(k in c(1:10)) {
  wss2[k] = kmeans(Group1.1[,-c(1,2,7,12,13)], k, nstart = 10)$tot.withinss
} #To find out the optimal number of clusters to use since high cluster no will make us prone to overfitting

plot(wss2, type ="b", xlab = "Number of clusters", "ylab" = "Total within-cluster sum of squares")
#Use 2 clusters as it results in the greatest fall in total within cluster sum of sqs.

cluster_no_vec <- kmeans(Group1.1[,-c(1,2,7,12,13)], 2, nstart=10)
Group1.1$Cluster_No3 <- cluster_no_vec[[1]] #Adding the cluster number back to the original dataframe

Group1.1.1 <- Group1.1[Group1.1$Cluster_No3 == 1,]  #7974 records
Group1.1.2 <- Group1.1[Group1.1$Cluster_No3 == 2,]  #269 records 

#Preparing the data to join together

Group2$Cluster_No = NULL
Group1.2$Cluster_No = NULL
Group1.2$Cluster_No2 = NULL

Group1.1.1$Cluster_No = NULL
Group1.1.1$Cluster_No2 = NULL
Group1.1.1$Cluster_No3 = NULL

Group1.1.2$Cluster_No = NULL
Group1.1.2$Cluster_No2 = NULL
Group1.1.2$Cluster_No3 = NULL

# reassign original df with the correct segment number. total no of segments = 4
Group2$`Segment_No` = 1  
Group1.2$`Segment_No` = 2
Group1.1.1$`Segment_No` = 4
Group1.1.2$`Segment_No` = 3

#clustered data
newdf = rbind(Group2,Group1.2,Group1.1.1,Group1.1.2)
library(openxlsx)
write.xlsx(newdf, "Clustered_Distinct_Influencer_Data.xlsx")


################NEED TO RUN WITH NEW DATA################

# Using a decision tree to generate classification rules to determine characteristics of influencers segment
df_clustered <- newdf
library(partykit)
library(rpart)
library(rpart.plot)
library(grid)
library(libcoin)
library(mvtnorm)

cart.tree1 <- rpart(Segment_No~., data=df_clustered[,-1], method='class', control=rpart.control(cp=0.01), model=TRUE)
par(mar = c(1, 1, 1, 1))
rpart.plot(cart.tree1, branch=0, split.cex=1.4, cex=.56, fallen.leaves=TRUE)

########## Looking at differences in summary statistics of variables and seeing if they are significant at the 95% level of significance #######
fseg <- as.factor(df_clustered$Segment_No)

#Looking at difference in follower count
tapply(df_clustered$followers_count, df_clustered$Segment_No, summary) # #Followers decreases from 1>2>3>4

your.aov.followers = aov(df_clustered$`followers_count`~df_clustered$Segment_No)
summary(your.aov.followers)
tuk_followers <- TukeyHSD(aov(df_clustered$followers_count~fseg))
tuk_followers #Significant difference across all segments

#Looking at difference in following count
tapply(df_clustered$following_count, df_clustered$Segment_No, summary) # #Following decreases from 4>2>3>1 (Median)

your.aov.following = aov(df_clustered$`following_count`~df_clustered$Segment_No)
summary(your.aov.following)
tuk_following <- TukeyHSD(aov(df_clustered$following_count~fseg))
tuk_following #No significant difference in following count between segment 1&2

#Looking at difference in likes count
tapply(df_clustered$likes_count, df_clustered$Segment_No, summary) # #Likes decreases from 4>2>3>1 (Median)

your.aov.likes= aov(df_clustered$`likes_count`~df_clustered$Segment_No)
summary(your.aov.likes)
tuk_likes <- TukeyHSD(aov(df_clustered$likes_count~fseg))
tuk_likes #No significant difference in following count between segment 1&2, 1&3, 2&3

#Looking at difference in max retweet count
tapply(df_clustered$max_retweet_value, df_clustered$Segment_No, summary) # #RTs decreases from 1>2>3>4 (Median)

your.aov.rt= aov(df_clustered$`max_retweet_value`~df_clustered$Segment_No)
summary(your.aov.rt)
tuk_rt <- TukeyHSD(aov(df_clustered$max_retweet_value~fseg))
tuk_rt #No significant difference in max retweet count between segment2&3

#Looking at difference in statuses count
tapply(df_clustered$statuses_count, df_clustered$Segment_No, summary) # #Statuses decreases from 1>2>3>4 (Median)

your.aov.stat= aov(df_clustered$`statuses_count`~df_clustered$Segment_No)
summary(your.aov.stat)
tuk_rt <- TukeyHSD(aov(df_clustered$statuses_count~fseg))
tuk_rt #No significant difference in status count between 1&2 

#Looking at difference in social ref count
tapply(df_clustered$socialrefcount, df_clustered$Segment_No, summary) # #Following decreases from 2>1>3>4 (Median)

your.aov.ref= aov(df_clustered$`socialrefcount`~df_clustered$Segment_No)
summary(your.aov.ref)
tuk_ref <- TukeyHSD(aov(df_clustered$socialrefcount~fseg))
tuk_ref #Significant difference across all segments 

#looking at difference in favourite count per post 
tapply(df_clustered$favourite_count_per_post, df_clustered$Segment_No, summary) # #Fav count decreases from 1>2>3>4

your.aov.fc= aov(df_clustered$favourite_count_per_post~df_clustered$Segment_No)
summary(your.aov.fc)
tuk_fc <- TukeyHSD(aov(df_clustered$favourite_count_per_post~fseg))
tuk_fc #Significant difference across all segments except 2-3

#looking at difference in RT count per post 
tapply(df_clustered$rt_count_per_post, df_clustered$Segment_No, summary) # #rt count decreases from 1>2>3>4

your.aov.rtpp= aov(df_clustered$rt_count_per_post~df_clustered$Segment_No)
summary(your.aov.rtpp)
tuk_rtpp <- TukeyHSD(aov(df_clustered$rt_count_per_post~fseg))
tuk_rtpp #Significant difference across all segments except 2-3

########## Publishers ##########
publishers_aggregated <- read_csv("C:/Users/65918/Desktop/publishers_aggregated.csv")
dfpub <- publishers_aggregated
dfpub <- dfpub[,2:14] #Removing publisher link in order to run clustering algorithm 

wsspub <- rep(NA, 10)

for(k in c(1:10)) {
  wsspub[k] = kmeans(dfpub, k, nstart = 10)$tot.withinss
} #To find out the optimal number of clusters to use since high cluster no will make us prone to overfitting

par(mar = c(4, 4, 4, 4))
plot(wsspub, type ="b", xlab = "Number of clusters", "ylab" = "Total within-cluster sum of squares")
#Use 2 clusters as it results in the greatest fall in total within cluster sum of sqs.

cluster_no_vec_pub <- kmeans(dfpub, 2, nstart=10)
dfpub$Cluster_No <- cluster_no_vec_pub[[1]] #Adding the cluster number back to the original dataframe

Group1 <- dfpub[dfpub$Cluster_No == 1,]  #4 records
Group2 <- dfpub[dfpub$Cluster_No == 2,]  #5076 records

wsspub2 <- rep(NA, 10)

for(k in c(1:10)) {
  wsspub2[k] = kmeans(Group2, k, nstart = 10)$tot.withinss
} #To find out the optimal number of clusters to use since high cluster no will make us prone to overfitting

plot(wsspub2, type ="b", xlab = "Number of clusters", "ylab" = "Total within-cluster sum of squares")

cluster_no_vec_pub2 <- kmeans(Group2, 2, nstart=10)
Group2$Cluster_No_L2 <- cluster_no_vec_pub2[[1]] #Adding the cluster number back to the original dataframe
table(Group2$Cluster_No_L2)

Group2.1 <- Group2[Group2$Cluster_No_L2 == 1,]  #28 records 
Group2.2 <- Group2[Group2$Cluster_No_L2 == 2,]  #5048 records 

wsspub3 <- rep(NA, 10)

for(k in c(1:10)) {
  wsspub3[k] = kmeans(Group2.2, k, nstart = 10)$tot.withinss
} #To find out the optimal number of clusters to use since high cluster no will make us prone to overfitting

plot(wsspub3, type ="b", xlab = "Number of clusters", "ylab" = "Total within-cluster sum of squares")

cluster_no_vec_pub3 <- kmeans(Group2.2, 2, nstart=10)
Group2.2$Cluster_No_L3 <- cluster_no_vec_pub3[[1]] #Adding the cluster number back to the original dataframe
table(Group2.2$Cluster_No_L3)

Group2.2.1 <- Group2.2[Group2.2$Cluster_No_L3 == 1,]  #151 records 
Group2.2.2 <- Group2.2[Group2.2$Cluster_No_L3 == 2,]  #4897 records 

Group1$Cluster_No = NULL
Group2.1$Cluster_No = NULL
Group2.1$Cluster_No_L2 = NULL

Group2.2.1$Cluster_No = NULL
Group2.2.1$Cluster_No_L2 = NULL
Group2.2.1$Cluster_No_L3 = NULL

Group2.2.2$Cluster_No = NULL
Group2.2.2$Cluster_No_L2 = NULL
Group2.2.2$Cluster_No_L3 = NULL

# reassign original df with the correct segment number. total no of segments = 4
Group1$`Cluster_No` = 1  
Group2.1$`Cluster_No` = 2
Group2.2.1$`Cluster_No` = 3
Group2.2.2$`Cluster_No` = 4

#clustered publishers data
newdf2 = rbind(Group1,Group2.1,Group2.2.1,Group2.2.2)
newdf2 = cbind(publishers_aggregated$publisher_link, newdf2)
df_clustered_publishers <-newdf2

fseg2 <- as.factor(df_clustered_publishers$Segment_No)

#Looking at difference in continent_code 
tapply(df_clustered_publishers$Continent_Code, df_clustered_publishers$Segment_No, summary) # 

your.aov.cc = aov(df_clustered_publishers$Continent_Code~df_clustered_publishers$Segment_No)
summary(your.aov.cc) #no significance difference in continent code
tuk_cc <- TukeyHSD(aov(df_clustered_publishers$Continent_Code~fseg2))
tuk_cc #No significance difference 

#Looking at difference in #Articles published 
tapply(df_clustered_publishers$Articles_Published, df_clustered_publishers$Segment_No, summary) # Median 1=3=4>2 ; Mean 4>3>2>1

your.aov.ap = aov(df_clustered_publishers$Articles_Published~df_clustered_publishers$Segment_No)
summary(your.aov.ap) #no significance difference in articles published
tuk_ap <- TukeyHSD(aov(df_clustered_publishers$Articles_Published~fseg2))
tuk_ap #No significance difference 

#Looking at difference in avg_tw_count
tapply(df_clustered_publishers$avg_tw_count, df_clustered_publishers$Segment_No, summary) # 1>2>3>4

your.aov.atc = aov(df_clustered_publishers$avg_tw_count~df_clustered_publishers$Segment_No)
summary(your.aov.atc) #Significance difference in avg_tw_count
tuk_atc <- TukeyHSD(aov(df_clustered_publishers$avg_tw_count~fseg2))
tuk_atc #Significant difference among all clusters

#Looking at difference in avg_pi_count
tapply(df_clustered_publishers$avg_pi_count, df_clustered_publishers$Segment_No, summary) # 1>2>3>4

your.aov.apc = aov(df_clustered_publishers$avg_pi_count~df_clustered_publishers$Segment_No)
summary(your.aov.apc) #Significance difference in avg_pi_count
tuk_apc <- TukeyHSD(aov(df_clustered_publishers$avg_pi_count~fseg2))
tuk_apc #Significant difference among all clusters except 2-3 and 2-4

#Looking at difference in avg_li_count
tapply(df_clustered_publishers$avg_li_count, df_clustered_publishers$Segment_No, summary) # 1>2>3>4

your.aov.alc = aov(df_clustered_publishers$avg_li_count~df_clustered_publishers$Segment_No)
summary(your.aov.alc) #Significance difference in avg_li_count
tuk_alc <- TukeyHSD(aov(df_clustered_publishers$avg_li_count~fseg2))
tuk_alc #Significant difference among all clusters except 2-3 

#Looking at difference in avg_fb_comments
tapply(df_clustered_publishers$avg_fb_comments, df_clustered_publishers$Segment_No, summary) # 2>3>4>1

your.aov.afbc = aov(df_clustered_publishers$avg_fb_comments~df_clustered_publishers$Segment_No)
summary(your.aov.afbc) #Significance difference in avg_fb_comments
tuk_afbc <- TukeyHSD(aov(df_clustered_publishers$avg_fb_comments~fseg2))
tuk_afbc #Significant difference among all clusters except 1-4

#Looking at difference in avg_fb_comments
tapply(df_clustered_publishers$avg_fb_comments, df_clustered_publishers$Segment_No, summary) # 2>3>4>1

your.aov.afbc = aov(df_clustered_publishers$avg_fb_comments~df_clustered_publishers$Segment_No)
summary(your.aov.afbc) #Significance difference in avg_fb_comments
tuk_afbc <- TukeyHSD(aov(df_clustered_publishers$avg_fb_comments~fseg2))
tuk_afbc #Significant difference among all clusters except 1-4

#Looking at difference in avg_fb_likes
tapply(df_clustered_publishers$avg_fb_likes, df_clustered_publishers$Segment_No, summary) # 2>3>4>1
your.aov.afbl = aov(df_clustered_publishers$avg_fb_likes~df_clustered_publishers$Segment_No)
summary(your.aov.afbl) #Significance difference in avg_fb_likes
tuk_afbl <- TukeyHSD(aov(df_clustered_publishers$avg_fb_likes~fseg2))
tuk_afbl #Significant difference among all clusters except 1-4

#Looking at difference in avg_fb_shares
tapply(df_clustered_publishers$avg_fb_shares, df_clustered_publishers$Segment_No, summary) # 2>3>4>1
your.aov.afbs = aov(df_clustered_publishers$avg_fb_shares~df_clustered_publishers$Segment_No)
summary(your.aov.afbs) #Significance difference in avg_fb_shares
tuk_afbs <- TukeyHSD(aov(df_clustered_publishers$avg_fb_shares~fseg2))
tuk_afbs #Significant difference among all clusters except 1-4

#Looking at difference in avg_fb_engagement
tapply(df_clustered_publishers$avg_fb_engagement, df_clustered_publishers$Segment_No, summary) # 1>2>3>4
your.aov.afbe = aov(df_clustered_publishers$avg_fb_engagement~df_clustered_publishers$Segment_No)
summary(your.aov.afbe) #Significance difference in avg_fb_engagement
tuk_afbe <- TukeyHSD(aov(df_clustered_publishers$avg_fb_engagement~fseg2))
tuk_afbe #Significant difference among all clusters 

#Looking at difference in avg_max_velocity
tapply(df_clustered_publishers$avg_max_velocity, df_clustered_publishers$Segment_No, summary) # median 2>1>3>4, mean 1>2>3>4
your.aov.amv = aov(df_clustered_publishers$avg_max_velocity~df_clustered_publishers$Segment_No)
summary(your.aov.amv) #Significance difference in avg_max_velocity
tuk_amv <- TukeyHSD(aov(df_clustered_publishers$avg_max_velocity~fseg2))
tuk_amv #Significant difference among all clusters except 1-2

#Looking at difference in avg_sentiment
tapply(df_clustered_publishers$avg_sentiment, df_clustered_publishers$Segment_No, summary) # most neg to positive 3>4>2>1
your.aov.as = aov(df_clustered_publishers$avg_sentiment~df_clustered_publishers$Segment_No)
summary(your.aov.as) #Significance difference in avg_sentiment 
tuk_as <- TukeyHSD(aov(df_clustered_publishers$avg_sentiment~fseg2))
tuk_as #Significant difference among 1-3, 3-4

#Looking at difference in avg_no_of_tweets_by_influencer
tapply(df_clustered_publishers$avg_no_of_tweets_by_influencer, df_clustered_publishers$Segment_No, summary) # 1>2>3>4
your.aov.anotbi = aov(df_clustered_publishers$avg_no_of_tweets_by_influencer~df_clustered_publishers$Segment_No)
summary(your.aov.anotbi) #Significance difference in avg_no_of_tweets_by_influencer
tuk_anotbi <- TukeyHSD(aov(df_clustered_publishers$avg_no_of_tweets_by_influencer~fseg2))
tuk_anotbi #Significant difference among all

#Looking at difference in avg_no_unique_twitter_influencer
tapply(df_clustered_publishers$avg_no_unique_twitter_influencer, df_clustered_publishers$Segment_No, summary) # 1>2>3>4
your.aov.anuti = aov(df_clustered_publishers$avg_no_unique_twitter_influencer~df_clustered_publishers$Segment_No)
summary(your.aov.anuti) #Significance difference in avg_no_unique_twitter_influencer
tuk_anuti <- TukeyHSD(aov(df_clustered_publishers$avg_no_unique_twitter_influencer~fseg2))
tuk_anuti #Significant difference among all

#3 How do you identify top publishers and influencers? 
#Which publishers and influencers should United Airlines pay attention to and why?

######### Influencers ###########

#we decided to use followers count and rt_per_post and fav_per_post as relevant attributes to
#determine top influence

#we want to put weights to indicate the relative importance of the 3 variables
#but first we are going to standardise the 3 columns
newdf <- df_clustered

followersmean = mean(newdf$followers_count)
followerssd = sqrt(var(newdf$followers_count))

fav_per_post_mean = mean(newdf$favourite_count_per_post)
fav_per_post_sd = sqrt(var(newdf$favourite_count_per_post))

rt_per_post_mean = mean(newdf$rt_count_per_post)
rt_per_post_sd = sqrt(var(newdf$rt_count_per_post))

newdfstandardised = newdf
newdfstandardised$followers = (newdfstandardised$followers_count - followersmean) / followerssd
newdfstandardised$fav_per_post = (newdfstandardised$favourite_count_per_post - fav_per_post_mean) / fav_per_post_sd
newdfstandardised$rt_per_post = (newdfstandardised$rt_count_per_post - rt_per_post_mean) / rt_per_post_sd

# We try to define a weighted score to define top influencers.
# depending on the number of followers, the weights will differ thus the score equation will differ.
newdfstandardised$Influencer_Score = NA

#Standardise dollowers count
standard5k <- (5000-followersmean)/followerssd
standard1k <- (1000-followersmean)/followerssd
standard10k <- (10000-followersmean)/followerssd
standard25k <- (25000-followersmean)/followerssd
standard50k <- (50000-followersmean)/followerssd
standard100k <- (100000-followersmean)/followerssd
standard250k <- (250000-followersmean)/followerssd
standard500k <- (500000-followersmean)/followerssd
standard1m <- (1000000- followersmean)/followerssd


#using different weights to give each influencer a score. The weights depends on the number of followers an influencer has.
newdfstandardised$Influencer_Score[newdfstandardised$followers < standard1k ] = (1/2) * newdfstandardised$fav_per_post[newdfstandardised$followers < standard1k ] + (1/2) * newdfstandardised$rt_per_post[newdfstandardised$followers < standard1k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard1k & newdfstandardised$followers < standard5k ] = (21/221)*newdfstandardised$followers[newdfstandardised$followers >= standard1k & newdfstandardised$followers < standard5k ] + (100/221) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard1k & newdfstandardised$followers < standard5k ] + (100/221) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard1k & newdfstandardised$followers < standard5k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard5k & newdfstandardised$followers < standard10k ] = (29/529)*newdfstandardised$followers[newdfstandardised$followers >= standard5k & newdfstandardised$followers < standard10k ] + (250/529) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard5k & newdfstandardised$followers < standard10k ] + (250/529) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard5k & newdfstandardised$followers < standard10k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard10k & newdfstandardised$followers < standard25k ] = (37/1037)*newdfstandardised$followers[newdfstandardised$followers >= standard10k & newdfstandardised$followers < standard25k ] + (500/1037) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard10k & newdfstandardised$followers < standard25k ] + (500/1037) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard10k & newdfstandardised$followers < standard25k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard25k & newdfstandardised$followers < standard50k ] = (3/103)*newdfstandardised$followers[newdfstandardised$followers >= standard25k & newdfstandardised$followers < standard50k ] + (50/103) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard25k & newdfstandardised$followers < standard50k ] + (50/103) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard25k & newdfstandardised$followers < standard50k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard50k & newdfstandardised$followers < standard100k ] = (27/1027)*newdfstandardised$followers[newdfstandardised$followers >= standard50k & newdfstandardised$followers < standard100k ] + (500/1027) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard50k & newdfstandardised$followers < standard100k ] + (500/1027) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard50k & newdfstandardised$followers < standard100k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard100k & newdfstandardised$followers < standard250k ] = (13/513)*newdfstandardised$followers[newdfstandardised$followers >= standard100k & newdfstandardised$followers < standard250k ] + (250/513) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard100k & newdfstandardised$followers < standard250k ] + (250/513) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard100k & newdfstandardised$followers < standard250k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard250k & newdfstandardised$followers < standard500k ] = (27/1027)*newdfstandardised$followers[newdfstandardised$followers >= standard250k & newdfstandardised$followers < standard500k] + (500/1027) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard250k & newdfstandardised$followers < standard500k ] + (500/1027) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard250k & newdfstandardised$followers < standard500k ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard500k & newdfstandardised$followers < standard1m ] = (3/128)*newdfstandardised$followers[newdfstandardised$followers >= standard500k & newdfstandardised$followers < standard1m ] + (125/256) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard500k & newdfstandardised$followers < standard1m ] + (125/256) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard500k & newdfstandardised$followers < standard1m ]
newdfstandardised$Influencer_Score[newdfstandardised$followers >= standard1m  ] = (11/511) * newdfstandardised$followers[newdfstandardised$followers >= standard1m] + (250/511) * newdfstandardised$fav_per_post[newdfstandardised$followers >= standard1m] + (250/511) * newdfstandardised$rt_per_post[newdfstandardised$followers >= standard1m]


#order the df by decreasing influencer score --> can identify top influencers
newdfstandardised_sorted = newdfstandardised[order(-newdfstandardised$Influencer_Score),]
write.csv(newdfstandardised_sorted, 'Distinct_Influencer_Scores.csv')

######### Publishers #########

# Normalising the variables so we can compare them using the same units 

df_pub_normalised <- subset(df_clustered_publishers, select=-c(Continent_Code, avg_sentiment, Segment_No))

ap_mean = mean(df_pub_normalised$Articles_Published)
ap_sd = sqrt(var(df_pub_normalised$Articles_Published))

atc_mean = mean(df_pub_normalised$avg_tw_count)
atc_sd = sqrt(var(df_pub_normalised$avg_tw_count))

apc_mean = mean(df_pub_normalised$avg_pi_count)
apc_sd = sqrt(var(df_pub_normalised$avg_pi_count))

alc_mean = mean(df_pub_normalised$avg_li_count)
alc_sd = sqrt(var(df_pub_normalised$avg_li_count))

afbc_mean = mean(df_pub_normalised$avg_fb_comments)
afbc_sd = sqrt(var(df_pub_normalised$avg_fb_comments))

afbl_mean = mean(df_pub_normalised$avg_fb_likes)
afbl_sd = sqrt(var(df_pub_normalised$avg_fb_likes))

afbs_mean = mean(df_pub_normalised$avg_fb_shares)
afbs_sd = sqrt(var(df_pub_normalised$avg_fb_shares))

afbe_mean = mean(df_pub_normalised$avg_fb_engagement)
afbe_sd = sqrt(var(df_pub_normalised$avg_fb_engagement))

amv_mean = mean(df_pub_normalised$avg_max_velocity)
amv_sd = sqrt(var(df_pub_normalised$avg_max_velocity))

anotbi_mean = mean(df_pub_normalised$avg_no_of_tweets_by_influencer)
anotbi_sd = sqrt(var(df_pub_normalised$avg_no_unique_twitter_influencer))

anuti_mean = mean(df_pub_normalised$avg_no_unique_twitter_influencer)
anuti_sd = sqrt(var(df_pub_normalised$avg_no_unique_twitter_influencer))

df_pub_normalised$Articles_Published = (df_pub_normalised$Articles_Published - ap_mean) /ap_sd
df_pub_normalised$avg_tw_count = (df_pub_normalised$avg_tw_count - atc_mean) /atc_sd
df_pub_normalised$avg_pi_count = (df_pub_normalised$avg_pi_count - apc_mean) /apc_sd
df_pub_normalised$avg_li_count = (df_pub_normalised$avg_li_count - alc_mean) /alc_sd
df_pub_normalised$avg_fb_comments = (df_pub_normalised$avg_fb_comments - afbc_mean) /afbc_sd
df_pub_normalised$avg_fb_likes = (df_pub_normalised$avg_fb_likes - afbl_mean) /afbl_sd
df_pub_normalised$avg_fb_shares = (df_pub_normalised$avg_fb_shares - afbs_mean) /afbs_sd
df_pub_normalised$avg_fb_engagement = (df_pub_normalised$avg_fb_engagement - afbe_mean) /afbe_sd
df_pub_normalised$avg_max_velocity = (df_pub_normalised$avg_max_velocity - amv_mean) /amv_sd
df_pub_normalised$avg_no_of_tweets_by_influencer = (df_pub_normalised$avg_no_of_tweets_by_influencer - anotbi_mean) /anotbi_sd
df_pub_normalised$avg_no_unique_twitter_influencer = (df_pub_normalised$avg_no_unique_twitter_influencer - anuti_mean) /anuti_sd

df_pub_normalised$Publisher_Score <- NA
df_pub_normalised$Publisher_Score <- (df_pub_normalised$Articles_Published + df_pub_normalised$avg_tw_count + df_pub_normalised$avg_pi_count
                                      + df_pub_normalised$avg_li_count + df_pub_normalised$avg_fb_comments + df_pub_normalised$avg_fb_likes
                                      + df_pub_normalised$avg_fb_shares + df_pub_normalised$avg_fb_engagement + df_pub_normalised$avg_max_velocity
                                      + df_pub_normalised$avg_no_of_tweets_by_influencer + df_pub_normalised$avg_no_unique_twitter_influencer)

#Added all the normalised scores together sicne we cannot decide what is the best weighs to assign to each variable

sorted <- df_pub_normalised[order(-df_pub_normalised$Publisher_Score),]
write.csv(sorted, 'Publisher_scores.csv')

###### WE TRIED THIS ######

#Hierachal clustering
PLE.Attr <- df1
hc.single <- hclust(dist(PLE.Attr), method = "single")
# Plot the dendrograms 
par(mfrow = c(1,1)) 
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = 0.07) 
d = as.dendrogram(hc.single)
plot(cut(d, h = 500000)$upper, main = "Single Linkage", xlab = "", sub = "", cex = 0.00000007)

max(df1$followers_count)
#using average linkage clustering
hc.cluster = cutree(hc.average, 4) #split into 4 clusters
hc.cluster
table(hc.cluster,rownames(PLE.Attr)) #finding the frequency 

#plotting the cut on the dendogram
par(mfrow = c(1,1))
plot(hc.average, labels = rownames(PLE.Attr), cex = 0.6, main = "Dendrogram for Average Linkage")
abline(h=8, col = "red")
aggregate(PLE.Attr, list(hc.cluster), median)
cor(PLE.Attr)

###### WE TRIED THIS #############

#Hierachal clustering
'''dfpub1<-publishers_aggregated
PLE.Attr <- dfpub[,(2:10)]
hc.single <- hclust(dist(PLE.Attr), method = "single")
# Plot the dendrograms 
par(mfrow = c(1,1)) 
d = as.dendrogram(hc.single)
plot(d, main = "Single Linkage", xlab = "", sub = "")

hc.cluster = cutree(hc.single, 10)
table(hc.cluster)
#using average linkage clustering
hc.cluster = cutree(hc.average, 20) #split into 4 clusters
hc.cluster
table(hc.cluster,rownames(PLE.Attr)) #finding the frequency 

#plotting the cut on the dendogram
par(mfrow = c(1,1))
plot(hc.average, labels = rownames(PLE.Attr), cex = 0.6, main = "Dendrogram for Average Linkage")
abline(h=8, col = "red")
aggregate(PLE.Attr, list(hc.cluster), median)
cor(PLE.Attr)'''