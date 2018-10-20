
library(clue)
library('factoextra')
library('cluster')
library('NbClust')
library("magrittr")
library(caTools)
library(plotly)
library(serials)
library(dplyr)
library(cluster)
library(gsubfn) 
library("StatMatch")

leadin <- read.csv(file="ASSETS/all_networks_episodes_leadin.csv", header=TRUE, sep="|")
delivery <- read.csv(file="ASSETS/all_networks_episodes_delivery.csv", header=TRUE, sep="|")

#Preparing data
######################################################################
delivery <-merge(x = delivery, y = leadin[,c("PREMIERE_PROGRAM_ID","REPORTED_START_TIME","DELIVERY_CONTR_LSD")],by.x = c("PROGRAM_ID"),by.y =  c("PREMIERE_PROGRAM_ID"))
delivery$CLEAR_DELIVERY <- delivery$DELIVERY-delivery$DELIVERY_CONTR_LSD
delivery <- delivery %>% group_by(SEASON,PROGRAM_NAME,PROGRAM_DISTRIBUTOR) %>% arrange(BROADCAST_DATE,REPORTED_START_TIME) %>% mutate(EPISODE_NUMBER=row_number())
data <- delivery[delivery[,("SEASON")]==1,]
tmp <- (data  %>% count(PROGRAM_DISTRIBUTOR,PROGRAM_NAME))
data <- merge(x = tmp, y = data,by = c("PROGRAM_NAME","PROGRAM_DISTRIBUTOR"))
rm(tmp)
colnames(data)[which(names(data) == "n")] <- "Number_of_episodes"
data$DURATION <-cut(data$REPORTEDDURATION,breaks=c(0,45,900),labels=c(0,1))
data <- data[,c("PROGRAM_ID", "PROGRAM_NAME","PROGRAM_DISTRIBUTOR","EPISODE_NUMBER","DURATION","Number_of_episodes","CLEAR_DELIVERY","DELIVERY_CONTR_LSD")]
data <- data[data[,"Number_of_episodes"]>2,]
data <- data[data[,"Number_of_episodes"]<=25,]
data$Number_of_episodes_new <- cut(data$Number_of_episodes,breaks=c(2,5,7,9,11,15,25))
data$Number_of_episodes_new_num<- as.numeric(data$Number_of_episodes)
data$PROGRAM_DISTRIBUTOR <- trimws(data$PROGRAM_DISTRIBUTOR)


#Separating high and low delivery serials
######################################################################
#tmp <- data %>% group_by(PROGRAM_NAME,PROGRAM_DISTRIBUTOR) %>% summarise_at(.funs=mean,.vars="DELIVERY")
data <- data[complete.cases(data), ]
tmp<-data[data$EPISODE_NUMBER==1,]
high_del_prog <- data[paste(data$PROGRAM_NAME,data$PROGRAM_DISTRIBUTOR) %in% paste((tmp[tmp$CLEAR_DELIVERY>quantile(data$CLEAR_DELIVERY,0.82),]$PROGRAM_NAME),(tmp[tmp$CLEAR_DELIVERY>quantile(data$CLEAR_DELIVERY,0.82),]$PROGRAM_DISTRIBUTOR)),]

low_del_prog <- data[paste(data$PROGRAM_NAME,data$PROGRAM_DISTRIBUTOR) %in%
                       paste((tmp[tmp$CLEAR_DELIVERY>50 & tmp$CLEAR_DELIVERY <=quantile(data$CLEAR_DELIVERY,0.82),]$PROGRAM_NAME),tmp[tmp$CLEAR_DELIVERY<=quantile(data$CLEAR_DELIVERY,0.82) & tmp$CLEAR_DELIVERY>50,]$PROGRAM_DISTRIBUTOR),]




#CLUSTERING 
#####################################################################

sub1<-function(data,i,k=i){
  data[data$Number_of_episodes>=k & data$EPISODE_NUMBER==i,][(paste(data[data$Number_of_episodes>=k & data$EPISODE_NUMBER==i,]$PROGRAM_NAME,data[data$Number_of_episodes>=k & data$EPISODE_NUMBER==i,]$PROGRAM_DISTRIBUTOR) %in% paste(data[data$Number_of_episodes>=k & data$EPISODE_NUMBER==1,]$PROGRAM_NAME,data[data$Number_of_episodes>=k &data$EPISODE_NUMBER==1,]$PROGRAM_DISTRIBUTOR)),]
}

sub <- function(datai,i,k){
  datai[datai$Number_of_episodes>i & datai$EPISODE_NUMBER == i+1,][(
    paste(datai[datai$Number_of_episodes>i &datai$EPISODE_NUMBER==i+1,]$PROGRAM_NAME,
          datai[datai$Number_of_episodes>i & datai$EPISODE_NUMBER==i+1,]$PROGRAM_DISTRIBUTOR)
    %in%
      paste(datai[datai$Number_of_episodes>i &datai$EPISODE_NUMBER==i & datai$CLUSTER==k,]$PROGRAM_NAME,
            datai[datai$Number_of_episodes>i &datai$EPISODE_NUMBER==i & datai$CLUSTER==k,]$PROGRAM_DISTRIBUTOR)
  ),]
}


num_of_clust_epi<-9
num_of_clust_del<-7

clustering_train <- function(train,num_of_clust_epi,num_of_clust_del){
      kmeans_vect <- list(NULL)
      train[,"CLUSTER_epi"]<- rep(0,NROW(train))
      gower_dist <- daisy(rbind(train[train$EPISODE_NUMBER==1,][,c("Number_of_episodes_new_num", "DURATION")]) ,metric = "gower")
      gower_mat <- as.matrix(gower_dist)
      pamx <- pam(gower_mat, k=num_of_clust_epi,diss=TRUE)
      train[train$EPISODE_NUMBER==1,][,"CLUSTER_epi"]<-pamx$clustering
      for(i in 2:25){
        train[train$PROGRAM_ID %in% sub1(train,i)$PROGRAM_ID ,]$CLUSTER_epi <- sub1(train,1,i)$CLUSTER_epi
      }
      train[,"CLUSTER_del"]<-rep(0,NROW(train))
      train[,"CLUSTER"] <- rep(0,NROW(train))
      for(i in 1:23){
        gower_mat <- as.matrix(c(train[train$EPISODE_NUMBER==i ,]$CLEAR_DELIVERY))
        kmean <- kmeans(gower_mat, num_of_clust_del)
        train[train$EPISODE_NUMBER==i,][,"CLUSTER_del"] <- kmean$cluster
        for(k in 1:num_of_clust_epi)
          for(j in 1:num_of_clust_del){
            if(NROW(train[train$CLUSTER_epi==k & train$CLUSTER_del==j,])!=0)
              train[train$CLUSTER_epi==k & train$CLUSTER_del==j,][,"CLUSTER"] <- k*num_of_clust_del+j-num_of_clust_del
          }
        kmeans_vect[[i]] <- kmean
      }
      return(list(train,pamx,kmeans_vect))
}

a <- clustering_train(high_del_prog,num_of_clust_epi,num_of_clust_del)

train_high <- a[[1]]
pamx_high <- a[[2]]
kmeans_vect_high <- a[[3]]

a <- clustering_train(low_del_prog,num_of_clust_epi,num_of_clust_del)
train_low <- a[[1]]
pamx_low <- a[[2]]
kmeans_vect_low <- a[[3]]

rm(a)

save(pamx_high,file="first_season_data/pamx_high.rda")
save(kmeans_vect_high,file="first_season_data/kmeans_vect_high.rda")
save(train_high, file = "first_season_data/train_high.rda")

save(pamx_low,file="first_season_data/pamx_low.rda")
save(kmeans_vect_low,file="first_season_data/kmeans_vect_low.rda")
save(train_low, file = "first_season_data/train_low.rda")