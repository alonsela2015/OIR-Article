# r file for figure of spreading groups in article with orit
# the original file is in Irad`s server, 
# C:\Users\Royl\Desktop\Temp-AlonSela\ALONS_ANALISIS_21.7.15.r


install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("stringi")

library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(stringi)

###########################  Alon - set random seed
now <- Sys.time()
options(digits.secs=6) 
seed<-substr(now,24, 25)
set.seed(as.numeric(seed)) 
###########################  

### read data and main aggregations ##
#setwd("C:/Users/Royl/Desktop/Temp-AlonSela")
setwd("C:/Users/user/Dropbox/ARTICLES/Orit Milo Cohen/article/fig")
userData<-read.csv("agg_user.csv",header=TRUE)
setwd("C:/Users/Alon/Documents/tmp/tmp/OritCohenMilo")

#this is the data received from twitter api
twitData<-read.csv("twitShort.csv",header=TRUE, nrows = 200)
counTwit<-read.csv("aggdat.df.csv", header = TRUE, nrows = 200)


#limit <- 800
  #define thresholds/constants
  HIGH_VALUE = 700
  LOW_MIN_VALUE = 100
  LOW_MAX_VALUE = 400
  TWEETS_SAMPLE_VALUE = 20
  USER_SAMPLE_VALUE = 10
  
#for (limit in seq(10, 910, 100)) {
  #twitDataCountRE=data.frame(aggregate(twitData$created_at~twitData$tweet_text,twitData ,length))
  #aggdat.df=data.frame(aggregate(rs$created_at~rs$tweet_text,rs ,length))
  
  # Define T_h / T_l = twit_HIGH / LOW
  T_h <-subset(counTwit,counTwit$count > HIGH_VALUE)
  T_l <-subset(counTwit,counTwit$count < LOW_MAX_VALUE & counTwit$count > LOW_MIN_VALUE)
 
   # Remove two first extra high tweets and sort by <count> DESC
   T_h<- T_h[with(T_h, order(-count)), ] 
   T_h<- T_h[1:(dim(T_h)[1] - 2),]
   T_l<- T_l[with(T_l, order(-count)), ] 
  
# adjusts other low dataframe to the lenght of the larger to be of same lengths
  dmth<-dim(T_h)[1]
  dmtl<-dim(T_l)[1]
  if (dmth != dmtl)  { 
    if (dmth > dmtl) T_h <- data.frame(T_h[1:dmtl,]) 
    if (dmth < dmtl) T_l <- data.frame(T_l[1:dmth,])
  }
  
  # define all initial tweet users that appear in the High/LOW twits: T_h / T_l as U_h/U_l
  T_U_h <- merge(x = T_h, y = twitData, by.x = "text", by.y = "tweet_text")
  T_U_l <- merge(x = T_l, y = twitData, by.x = "text", by.y = "tweet_text")
  
  # sort by $twit then by $created_at
  T_U_h<- T_U_h[with(T_U_h, order(text, created_at)), ] # sorted tweets by created_at   
  T_U_l<- T_U_l[with(T_U_l, order(text, created_at)), ] 
  
  ##########################################################################
  # fetch k first instances out of High/ Low for each tweet
  # This part uses the super fast data.table package.
  
  res<-data.frame()
  res_o_CM<-data.frame()
    for (K_FIRST in seq(1, LOW_MAX_VALUE, 10)){
        #  K_FIRST = 5   #DEBUG
        # create two sets of K_SAMPLE users from each tweet, from HIGH and LOW, thus forming two even sized sets from the first spreaders
        F_T_U_h <- data.frame(setDT(T_U_h)[,.SD[1:K_FIRST] , text])    # fetch K_FIRST users from each twit in High
        F_T_U_l <- data.frame(setDT(T_U_l)[,.SD[1:K_FIRST] , text])    # fetch K_FIRST users from each twit in Low
        
        h<-data.frame(unique(F_T_U_h$user_id))      # unque K_FIRST users from each tweet in High               
        l<-data.frame(unique(F_T_U_l$user_id))      # unque K_FIRST users from each tweet in Low  
        
        #prop_h <- (dim(data.frame(h)) / [dim(data.frame(F_T_U_h)])) #Proportion of repeting names in High
        #prop_l <- (dim(data.frame(l)) / dim(data.frame(F_T_U_l))) #Proportion of repeting names in Low
        
        prob_h <- (dim(F_T_U_h)[1] - dim(h)[1]) / dim(data.frame(F_T_U_h))[1] 
        prob_l <- (dim(F_T_U_l)[1] - dim(l)[1]) / dim(data.frame(F_T_U_l))[1] 
      
        prob_h_o_CM <- (dim(h)[1] / dim(F_T_U_h)[1])   
        prob_l_o_CM <- (dim(l)[1]  / dim(F_T_U_l)[1])

        res <- rbind(res,c(K_FIRST, prob_h,prob_l))
        res_o_CM <- rbind(res_o_CM,c(K_FIRST, prob_h_o_CM,prob_l_o_CM))
  }
  colnames(res) <- c("prop_sampled", "High", "Low")
  
  Graphsubtitle<-paste("Repeating names \n in different sample sizes")
  
  ggplot(res, aes(x=res$prop_sampled)) +                    # basic graphical object
    geom_line(aes(y=res$High), colour="red") +  # first layer
    geom_line(aes(y=res$Low), colour="green") + # second layer
    ylab("% of repeting names") + xlab("Sample Size") +
    #ggtitle("Repeting Names of High spreaders Compared to Low Spreaders \n"Graphsubtitle) +
    ggtitle(Graphsubtitle) +
    geom_point(aes(y=res$High), colour="red", shape = 1) + 
    geom_point(aes(y=res$Low), colour="green", shape = 2)  
#####################################################################################
  
#}
colnames(res) <- c("prop_sampled", "High", "Low")

melt_res<-melt(res, id.vars = "prop_sampled")  # the X axis is in id.vars
ggplot(melt_res, aes(x=prop_sampled, y=value, colour=variable,  linetype=variable)) +
  geom_line() +
  geom_point(shape=c(22), size=1
             , fill="white") + 
  ylab("% of repeting \n spreader names") + xlab("Sample size (k)") +  
  ggtitle("Repeating Spreaders (%) in High vs. Low Groups")

##################################  ORIT CHANGE IN ANALYSIS   ################################
colnames(res_o_CM) <- c("prop_sampled", "High", "Low")

melt_res<-melt(res_o_CM, id.vars = "prop_sampled")  # the X axis is in id.vars
ggplot(melt_res, aes(x=prop_sampled, y=value, colour=variable,  linetype=variable)) +
  geom_line() +
  geom_point(shape=c(22), size=2
             , fill="white") + 
  ylab("% of repeting \n spreader names") + xlab("Sample size (k)") +  
  ggtitle("Repeting Spreaders (%) in High vs. Low Groups (Orit`s method)")

###########################################################################






#check what are the names that reappear for k=150

F_T_U_h <- data.frame(setDT(T_U_h)[,.SD[1:150] , text]) 
F_T_U_l <- data.frame(setDT(T_U_l)[,.SD[1:150] , text]) 
name_h<-table(F_T_U_h$user_id)
name_l<-table(F_T_U_l$user_id)
name_h<-data.frame(name_h)
name_l<-data.frame(name_l)
name_h<- name_h[with(name_h, order(-Freq)), ]
name_l<- name_l[with(name_l, order(-Freq)), ]

# plot2DataVal<-c(F_T_U_l$user_id,F_T_U_h$user_id)
# plot2Data<-data.frame(cbind(c(F_T_U_l$user_id,F_T_U_h$user_id),c(rep("Low",dim(F_T_U_h)[1]),rep("High",dim(F_T_U_h)[1]))) )
# colnames(plot2Data[,]) <- c("User", "Model")

#melt_Plot2Data<-melt(plot2Data, id.vars = "prop_sampled")
# ggplot(plot2Data, aes(x=X1, linetype=X2)) + geom_density(alpha=.3)

##########################################################################
# sample k out of high / low (not first k but just k)
# res<-data.frame()
# for (j in seq(10, 120, 5)){
# 
# K_SAMPLE = j # how many k sampled users that spread a twit do we want to fetch
# #K_FIRST = 30 # how many first users that spread a twit do we want to fetch
# 
# # create two sets of K_SAMPLE users from each tweet, from HIGH and LOW, thus forming an even sized sets
# K_T_U_h<-T_U_h %>% group_by(text) %>% dplyr:::sample_n.grouped_df(size = K_SAMPLE)
# K_T_U_l<-T_U_l %>% group_by(text) %>% dplyr:::sample_n.grouped_df(size = K_SAMPLE)
# 
# h<-unique(K_T_U_h$user_id)
# l<-unique(K_T_U_l$user_id)
#           
# prop_h <- (dim(data.frame(h)) / dim(data.frame(K_T_U_h)))
# prop_l <- (dim(data.frame(l)) / dim(data.frame(K_T_U_l)))
# 
# res<-rbind(res,c(j, prop_h[1],prop_l[1]))
# }
# colnames(res) <- c("num_sampled", "High", "Low")
# 
# plot(res$num_sampled,res$High, ylim = c(0,1))
# plot(res$num_sampled,res$Low, ylim = c(0,1))


###############################################
###############################################
###############################################

setwd("C:/Users/Royl/Desktop/Temp-AlonSela")

saveRDS(rs, file="mytweets.rds")
rs <- readRDS("mytweets.rds")


x<-rs$tweet_text
y<-x[1:1000]
z<-x[762:780]
FUN<-function(x) {
  a<-regmatches(x, gregexpr('(?<=@).*?(?=:)', x, perl=T))[[1]]
  b<-data.frame[1]
  return(b)
}
   
allNames<-lapply(y,FUN)
allNames1<-allNames[1:1000]

write.csv(allNames, file = "allNames.csv",row.names=TRUE, na="")
# install.packages('tm')
# library(tm)
# corpus<-data.frame(allNames)
# corpus <- Corpus(allNames)
# dtm <- DocumentTermMatrix(corpus)
# dtm2 <- as.matrix(dtm)
# frequency <- colSums(dtm2)
# frequency <- sort(frequency, decreasing=TRUE)
# head(frequency)
# 
# 
# install.packages('wordcloud')

tmp<-counTwit[counTwit$count>10,]


#################################################################
###               fig3 
###################################################################
setwd("C:/Users/Royl/Desktop/Temp-AlonSela")
Imp2Spread<-read.csv("importance vs repetitions.csv",header=TRUE)
# lm_eqn = function(df){
#   m = lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
# }

ggplot(Imp2Spread, aes(x=IMPORTANCE, y=Repetitions)) + geom_point(shape=1) + geom_smooth(method=lm,se=TRUE) +  # Add linear regression lines
  geom_text(x = 25, y = 300, label = lm_eqn(Imp2Spread), parse = TRUE)

ggplot(Imp2Spread, aes(x=IMPORTANCE, y=Repetitions)) + geom_point(shape=1) + geom_smooth(method=lm,se=TRUE) +  # Add linear regression lines
  geom_text(x = 25, y = 300, label = lm_eqn(Imp2Spread), parse = TRUE)
#p1 = p + geom_text(aes(x = 25, y = 300, label = lm_eqn(df)), parse = TRUE)

