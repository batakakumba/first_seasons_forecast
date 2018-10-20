# first_seasons_forecast
Predicting views for the first seasons of various shows

First step - data preparation:

Feature selection.

    - DURATION: 0 - if serial duration is less then 45 min
    
                1 - otherwise
                
   #according to research we can distinguish two groups of serials: 30-40 min length and 60-90 minutes length
   
    - Number of episodes - number of episodes in first season
    
    - Clear delivery - views per minutes
 
Clusterization consist of two steps

1. Pam algorithm on gower distance of categorical features: number of episodes and duration.

2. Kmeans on clear delivery.

We compute clasterization separately on first episodes,then on second episodes and etc

Forecast.

In order to select most similar shows in each group we use correlation between clear delivery of episodes for
this shows for all episodes starting from second.

During prediction we compute distances between episode and clusters centroids and choosing the closest one  and then perfom correlation 
analisys.
