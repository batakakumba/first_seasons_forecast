

library(clue)
library('factoextra')
library('cluster')
library('NbClust')
library("magrittr")
library(caTools)
library(plotly)
library(dplyr)
library(cluster)
library(gsubfn)
library("StatMatch")

##Loading data

serials <- readRDS(file = "ASSETS/episodes.rds")
serials <- serials[serials$Season == 1, ]
serials$Episode <-
  as.integer(substr(serials$Episode, 1, 3)) - as.integer(substr(serials$Season, 1, 1)) *
  100
colnames(serials) <- toupper(colnames(serials))
colnames(serials)[which(names(serials) == "SHOW")] <- "PROGRAM_NAME"
colnames(serials)[which(names(serials) == "EPISODE")] <- "EPISODE_NUMBER"
colnames(serials)[which(names(serials) == "DELIVERY_LEADIN")] <- "DELIVERY_CONTR_LSD"
serials <- serials %>% group_by(PROGRAM_NAME) %>% arrange(EPISODE_NUMBER) %>% mutate(EPISODE_NUMBER =
                                                                              row_number())
serials <- serials[order(serials$EPISODE_NUMBER),]
tmp <- (serials  %>% count(PROGRAM_NAME))
serials <- merge(x = tmp,
                 y = serials,
                 by = c("PROGRAM_NAME"))
colnames(serials)[which(names(serials) == "n")] <-  "Number_of_episodes"
serials$CLEAR_DELIVERY <- serials$DELIVERY - serials$DELIVERY_CONTR_LSD
serials$PROGRAM_DISTRIBUTOR <- rep("AMC", NROW(serials))
serials$DURATION <- cut(serials$DURATION,
          breaks = c(0, 45, 900),
          labels = c(0, 1))
serials <- serials[, c(
    "PROGRAM_NAME",
    "PROGRAM_DISTRIBUTOR",
    "EPISODE_NUMBER",
    "DURATION",
    "Number_of_episodes",
    "CLEAR_DELIVERY",
    "DELIVERY_CONTR_LSD",
    "STATUS"
  )]
serials$Number_of_episodes_new <-
  cut(serials$Number_of_episodes, breaks = c(2, 5, 7, 9, 11, 15, 25))
serials$Number_of_episodes_new_num <-
  as.numeric(serials$Number_of_episodes)
rm(tmp)

#FORECAST
###############################################################

num_of_clust_epi <- 9
num_of_clust_del <- 7


forecast_with_reclust <-
  function(train,
           pamx,
           kmeans_vect,
           test,
           num_of_clust_epi,
           num_of_clust_del,
           top_k) {
    set.seed(123)
    n <- NROW(test)
    serial <- test
    #if delivery of some episodes are  equal, correlation won't work
    serial$CLEAR_DELIVERY[duplicated(serial$CLEAR_DELIVERY)] <-
      serial$CLEAR_DELIVERY[duplicated(serial$CLEAR_DELIVERY)] + seq(1, length(serial$CLEAR_DELIVERY[duplicated(serial$CLEAR_DELIVERY)])) * 0.01
    test <- serial
    #adding episodes rows which we going to predict
    if (n != test$Number_of_episodes[1]) {
      for (i in (n + 1):(test$Number_of_episodes[1]))
        test <- rbind(test, test[1, ])
      test[(n + 1):NROW(test), ]$CLEAR_DELIVERY <- rep(NA, test$Number_of_episodes[1] - n)
      test[(n + 1):NROW(test), ]$EPISODE_NUMBER <- (n + 1):(test$Number_of_episodes[1])
      
      #######################################
      #print("clustering episodes...")
      test[, "CLUSTER_epi"] <- rep(0, NROW(test))
      epi <- c(NULL)
      for (i in 1:NROW(test[test$EPISODE_NUMBER == 1, ])) {
        all_dist <- as.matrix(gower.dist(train[train$EPISODE_NUMBER == 1, ][, c("Number_of_episodes_new_num", "DURATION")][pamx$id.med, ],
                               test[test$EPISODE_NUMBER == 1, ][, c("Number_of_episodes_new_num", "DURATION")][i, ]))
        #it may occur that there are more then one clossest centroid, so we check which cluster have serials with the same number_of_episodes
        closest_centroids <- which(all_dist == min(all_dist), arr.ind = T)[, 1]
        for (j in 1:length(closest_centroids))
          if (test[test$EPISODE_NUMBER == 1, ]$Number_of_episodes %in% train[train$EPISODE_NUMBER ==
                                                                             1, ][train[train$EPISODE_NUMBER == 1, ]$CLUSTER_epi == closest_centroids[j], ]$Number_of_episodes)
            epi <- (closest_centroids[j])
      }
      test[test$EPISODE_NUMBER == 1, ][, "CLUSTER_epi"] <- as.vector(epi)
      test$CLUSTER_epi <- rep(test[test$EPISODE_NUMBER == 1, ][, "CLUSTER_epi"], test$Number_of_episodes[1])
      
      ########################################
      #   print("clustering CLEAR_DELIVERY...")
      test[, "CLUSTER_del"] <- rep(0, NROW(test))
      test[, "coeff"] <- rep(1, NROW(test))
      test[, "CLUSTER"] <- rep(0, NROW(test))
      
      for (i in n:(test$Number_of_episodes[1] - 1)) {
        test[test$EPISODE_NUMBER == i, ][, "CLUSTER_del"] <- cl_predict(kmeans_vect[[i]], as.matrix(c(test[test$EPISODE_NUMBER == i, ]$CLEAR_DELIVERY)))
        test[test$EPISODE_NUMBER == i, ][, "CLUSTER"] <-  test[test$EPISODE_NUMBER == i, ]$CLUSTER_epi * num_of_clust_del + test[test$EPISODE_NUMBER ==
                                                                                   i, ]$CLUSTER_del - num_of_clust_del
        #correlation isn't used on first episode
        if (i > 1) {
          train_in_clust_cur_epi <- train[train$Number_of_episodes > i & train$EPISODE_NUMBER == i & train$CLUSTER == test[test$EPISODE_NUMBER == i, ]$CLUSTER, ]
          test_del <- test[1:i, ]$CLEAR_DELIVERY
          train_in_clust <- train_in_clust_cur_epi[1, ]
          train_in_clust <- train_in_clust[-1, ]
          for (j in 1:(i + 1))
            train_in_clust <-
            rbind(train_in_clust, train[train$EPISODE_NUMBER == j &
                                          train$Number_of_episodes > i, ][paste(train[train$EPISODE_NUMBER == j &
                                                                                        train$Number_of_episodes > i, ]$PROGRAM_NAME, train[train$EPISODE_NUMBER ==
                                                                                                                                              j &
                                                                                                                                              train$Number_of_episodes > i, ]$PROGRAM_DISTRIBUTOR) %in% paste(
                                                                                                                                                train_in_clust_cur_epi$PROGRAM_NAME,
                                                                                                                                                train_in_clust_cur_epi$PROGRAM_DISTRIBUTOR
                                                                                                                                              ), ])
          train_in_clust_del <- train_in_clust[train_in_clust$EPISODE_NUMBER == 1, ]$CLEAR_DELIVERY
          for (t in 2:(i))
              train_in_clust_del <- cbind(train_in_clust_del, train_in_clust[train_in_clust$EPISODE_NUMBER == t, ]$CLEAR_DELIVERY)
          train_in_clust_del <- as.data.frame(train_in_clust_del)
          #if delivery of some episodes are  equal, correlation won't work
          train_in_clust_del[train_in_clust_del[, 1] == train_in_clust_del[, 2], ][, 1] <-
            train_in_clust_del[train_in_clust_del[, 1] == train_in_clust_del[, 2], ][, 1] + 0.01
          train_in_clust_del <- t(train_in_clust_del)
          if (i == 2) {
            cor_res_TF <- cor(test_del, train_in_clust_del) > 0
            cor_res <- cor(test_del, train_in_clust_del)[cor(test_del, train_in_clust_del) > 0]
          }
          else  {
            cor_res <- tail(sort(cor(test_del, train_in_clust_del)), top_k)
            cor_res_TF <- cor(test_del, train_in_clust_del) %in% (cor_res)
            cor_res <- cor(test_del, train_in_clust_del)[cor(test_del, train_in_clust_del) %in% (cor_res)]
          }
          int <- train_in_clust[train_in_clust$EPISODE_NUMBER == i + 1, ][cor_res_TF, ]$CLEAR_DELIVERY
          coeff <- mean((int) / train_in_clust[train_in_clust$EPISODE_NUMBER == i, ][cor_res_TF, ]$CLEAR_DELIVERY - 1)
 
          
        }
        else{
          train_AMC <- train[train$PROGRAM_DISTRIBUTOR == "AMC", ]
          coeff <- mean(train_AMC[train_AMC$EPISODE_NUMBER == 2, ]$CLEAR_DELIVERY / train_AMC[train_AMC$EPISODE_NUMBER ==  1, ]$CLEAR_DELIVERY - 1)

        }
        if((n==1 & i==3) | (n>2 & i==n)){
          int <- train_in_clust[train_in_clust$EPISODE_NUMBER == (i + 1), ][cor_res_TF, ]
          save(int, file = paste("first_season_data/first_season_serials_to_compare/",paste(serial[1, ]$PROGRAM_NAME, "_serials_to_compare.rda"),sep = "",collapse = ""))
         
        }
    
        int1 <- test[test$EPISODE_NUMBER == i, ]
        test[test$EPISODE_NUMBER == i + 1, ]$CLEAR_DELIVERY <- int1$CLEAR_DELIVERY + coeff * int1$CLEAR_DELIVERY
        test[test$EPISODE_NUMBER == i + 1, ]$coeff <- coeff + 1
      }
      test
    }
    else
      print("ALL EPISODES ARE KNOWN!")
  }




save_serial_to_file <-  function(num_of_clust_epi, num_of_clust_del, train, pamx, kmeans_vect,  serial, cor_coef, top_k) {
    ind <- max(which(serial$STATUS == "Actual"))
    if (is.infinite(ind))
      ind <- 1
    f1 <- forecast_with_reclust(train,
                            pamx,
                            kmeans_vect,
                            serial[1:ind, c(1:6, 9, 10)],
                            num_of_clust_epi,
                            num_of_clust_del,
                            5)
    return(f1$coeff)
  }


##############################################################
rm(growth_episodic)
growth_episodic <- readRDS(file = "ASSETS/growth_episodic.rds")
a <- growth_episodic
load(file = "first_season_data/pamx_low.rda")
load(file = "first_season_data/kmeans_vect_low.rda")
load(file = "first_season_data/train_low.rda")
load(file = "first_season_data/pamx_high.rda")
load(file = "first_season_data/kmeans_vect_high.rda")
load(file = "first_season_data/train_high.rda")
delivery_separation <- 672
for (j in 1:length(unique(serials$PROGRAM_NAME))) {
  serial <- serials[serials$PROGRAM_NAME == unique(serials$PROGRAM_NAME)[j],]
  serial <- serial[order(serial$EPISODE_NUMBER),]
  if ((max(which(serial$STATUS == "Actual"))) != serial$Number_of_episodes[1]) {
    if (serial[serial$EPISODE_NUMBER == 1, ]$CLEAR_DELIVERY < delivery_separation) {
      train <- train_low
      pamx <- pamx_low
      kmeans_vect <- kmeans_vect_low
    } else{
      train <- train_high
      pamx <- pamx_high
      kmeans_vect <- kmeans_vect_high
    }
    
    my_forecast <- growth_episodic[1, ]
    my_forecast$Show <- serial$PROGRAM_NAME[1]
    my_forecast$Season <- 1
    pred <- save_serial_to_file(num_of_clust_epi,  num_of_clust_del,  train,  pamx, kmeans_vect, serial, 5)
    if (!is.infinite(min((which( serial$STATUS == "Actual"))))) {
      pred[1:max((which(serial$STATUS == "Actual")))] <-
        growth_episodic[growth_episodic$Season == 1, ][my_forecast$Show == growth_episodic[growth_episodic$Season ==
                                                                                             1, ]$Show, ][, 3:NCOL(my_forecast)][1:max((which(serial$STATUS == "Actual")))]
    }
    pred <- c(pred, rep(NA, 13 - length(pred)))
    my_forecast[, 3:NCOL(my_forecast)] <- pred
    if (my_forecast$Show %in% growth_episodic[growth_episodic$Season == 1, ]$Show)
      growth_episodic[growth_episodic$Season == 1, ][my_forecast$Show == growth_episodic[growth_episodic$Season == 1, ]$Show, ] <- my_forecast
    else{
      growth_episodic <- rbind(growth_episodic, my_forecast)
    }
  }
}

saveRDS(growth_episodic, file = "ASSETS/growth_episodic.rds")
