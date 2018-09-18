# still want to do:
#   check for other interaction effects.
#   work more with emmeans plotting functions... useful (check consider_emm.R) where ran single lm function and then some nice plots ...
# 
# load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(emmeans)
library(corrplot)
library(caret)
library(randomForest)
library(MASS)

# reading in the different datasets. NB...not using "simple" versions for now...Reconsider later.

# set95c <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/set95c.rds")
set02c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02c.rds")
set05c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05c.rds")
set08c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08c.rds")
set10c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10c.rds")
set12c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12c.rds")
set14c <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14c.rds")

# create a pooled set
# function to isolate relevant variables and add year
add_year <- function(set, year) {
  set[,c('qn',
         'cluster',
         'age',
         'age_actual',
         'sex',
         'edu',
         'hh_inc',
         'race',
         'lsm',
         'newspapers',
         'magazines',
         'radio',
         'tv',
         'internet',
         'all'
  )] %>%
    mutate(year = year)
}

# create pooled set
pooled_set <- rbind.data.frame(
  add_year(set02c, 2002),
  add_year(set05c, 2005),
  add_year(set08c, 2008),
  add_year(set10c, 2010),
  add_year(set12c, 2012),
  add_year(set14c, 2014)
)

# standardise
pooled_set[,10:14] <- scale(pooled_set[,10:14])

# some correlations:
corrplot02 <- ggcorrplot(cor(set02c[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
                         hc.order = FALSE,
                         type = "lower",
                         lab = TRUE,
                         title = "2002",
                         legend.title = "",
                         ggtheme = ggplot2::theme_minimal,
                         lab_size = 3,
                         tl.cex = 10,
                         show.legend = FALSE)
corrplot05 <- ggcorrplot(cor(set05c[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
                         hc.order = FALSE,
                         type = "lower",
                         lab = TRUE,
                         title = "2005",
                         legend.title = "",
                         ggtheme = ggplot2::theme_minimal,
                         lab_size = 3,
                         tl.cex = 10,
                         show.legend = FALSE)
corrplot08 <- ggcorrplot(cor(set08c[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
                         hc.order = FALSE,
                         type = "lower",
                         lab = TRUE,
                         title = "2008",
                         legend.title = "",
                         ggtheme = ggplot2::theme_minimal,
                         lab_size = 3,
                         tl.cex = 10,
                         show.legend = FALSE)
corrplot10 <- ggcorrplot(cor(set10c[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
                         hc.order = FALSE,
                         type = "lower",
                         lab = TRUE,
                         title = "2010",
                         legend.title = "",
                         ggtheme = ggplot2::theme_minimal,
                         lab_size = 3,
                         tl.cex = 10,
                         show.legend = FALSE)
corrplot12 <- ggcorrplot(cor(set12c[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
                         hc.order = FALSE,
                         type = "lower",
                         lab = TRUE,
                         title = "2012",
                         legend.title = "",
                         ggtheme = ggplot2::theme_minimal,
                         lab_size = 3,
                         tl.cex = 10,
                         show.legend = FALSE)
corrplot14 <- ggcorrplot(cor(set14c[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
                         hc.order = FALSE,
                         type = "lower",
                         lab = TRUE,
                         title = "2014",
                         legend.title = "",
                         ggtheme = ggplot2::theme_minimal,
                         lab_size = 3,
                         tl.cex = 10,
                         show.legend = FALSE)

jpeg("corrplots_by_year.jpeg")
grid.arrange(corrplot02,
             corrplot05,
             corrplot08,
             corrplot10,
             corrplot12,
             corrplot14,
             ncol = 2)
dev.off()

jpeg("corrplot_pooled.jpeg")
ggcorrplot(cor(pooled_set[,c("newspapers","magazines","radio", "tv", "internet", "all")]),
           hc.order = FALSE,
           type = "lower",
           lab = TRUE,
           title = "Pooled",
           legend.title = "",
           ggtheme = ggplot2::theme_minimal,
           lab_size = 3,
           tl.cex = 14,
           show.legend = TRUE)
dev.off()


# Clustering
## consider kmeans
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6,7,8,9,10)) {
  temp <- kmeans(pooled_set[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                 centers = k,
                 nstart = 5,
                 iter.max = 30)
  wss <- append(wss,temp$tot.withinss)
}

jpeg('kmeans_pooled.jpeg')
plot(c(1,2,3,4,5,6,7,8,9,10), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans_pooled <- kmeans(pooled_set[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 5,
                   iter.max = 100)

table(kmeans_pooled$cluster)


# add cluster variable to pooled set...
pooled_set_c <- pooled_set %>%
  mutate(cluster = factor(kmeans_pooled$cluster)) %>%
  dplyr::select(qn, cluster, everything())

# save it
saveRDS(pooled_set_c, "pooled_set_c.rds")

# read back
pooled_set_c <- readRDS("pooled_set_c.rds")

# multi-dimensional scaling

# 1st create a subset of 10 000 cases to ensure easier running
set.seed(56)
sub_pooled <- pooled_set_c[sample(nrow(pooled_set_c), size = 10000),]

# distance matrix and MDS
sub_pooled_dist <- dist(sub_pooled[,c("newspapers","magazines","radio", "tv", "internet", "all")])
# mds_pooled <- cmdscale(sub_pooled_dist)
# 
# saveRDS(mds_pooled, "mds_pooled.rds")
mds_pooled <- readRDS("mds_pooled.rds")

jpeg('kmeans_pooled_MDS.jpeg')
plot(mds_pooled, col = as.numeric(sub_pooled$cluster) + 1, pch = 19, ylab = "", xlab = "")
dev.off()

# predictions to test clusters...
# create training and test sets:
set.seed(56)
ind_train <- createDataPartition(pooled_set_c$cluster, p = 0.7, list = FALSE)
training <- pooled_set_c[ind_train,]
testing <- pooled_set_c[-ind_train,]

# A simple random forest:
forest_pooled_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest_pooled_type <- predict(forest_pooled_type, newdata = testing)

confusionMatrix(pred_forest_pooled_type, testing$cluster) 

# with linear discriminant analysis. Although given accuracy of forest,  no real need.
set.seed(56)
lda_pooled <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)

pred_lda_pooled <- predict(lda_pooled , newdata = testing)
confusionMatrix(pred_lda_pooled$class, testing$cluster) # collinearity meant took out 

# boxplots of clusters and media types
# define a plotting function
boxplot_clusters <- function(set,type) {
  ggplot(set, aes_string("cluster", type, fill = "cluster")) +
    geom_boxplot() +
    guides(fill = FALSE) +
    labs(title = type)
  
}

jpeg('typeBoxPlots_pooled.jpeg', quality = 100)
grid.arrange(boxplot_clusters(pooled_set_c, type = "all"),
             boxplot_clusters(pooled_set_c, type = "newspapers"),
             boxplot_clusters(pooled_set_c, type = "magazines"),
             boxplot_clusters(pooled_set_c, type = "radio"),
             boxplot_clusters(pooled_set_c, type = "tv"),
             boxplot_clusters(pooled_set_c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

# make sense of demographics

# set factor labels (NB double check levels)
pooled_set_c$age <- factor(pooled_set_c$age, labels = c("15-24","25-44", "45-54","55+"), ordered = TRUE)
pooled_set_c$race <- factor(pooled_set_c$race,labels = c("black", "coloured", "indian", "white"), ordered = FALSE)
pooled_set_c$edu <- factor(pooled_set_c$edu, labels = c("<matric", "matric",">matric" ) ,ordered = FALSE)
pooled_set_c$lsm <- factor(pooled_set_c$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = FALSE)
pooled_set_c$sex <- factor(pooled_set_c$sex, labels = c("male", "female"), ordered = FALSE)
pooled_set_c$hh_inc <- factor(pooled_set_c$hh_inc, labels = c("<R2500","R2500-R6999","R7000-R11999",">=R12000"), ordered = FALSE)
pooled_set_c$cluster <- factor(pooled_set_c$cluster,  labels = c("heavy", "internet", "medium", "light"), ordered = FALSE)
pooled_set_c$year <- factor(pooled_set_c$year, ordered = FALSE)


# size of each cluster
ggplot(data = pooled_set_c, aes(x = cluster, fill = cluster)) +
  geom_bar(stat = "count") +
  guides(fill = FALSE)

# demographics by cluster

bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
  if(category == "race") {
    level = c("black", "coloured", "indian", "white")
    title = "Population Group"
  }
  if(category == "edu") {
    level = c(c("<matric", "matric",">matric"))
    title = "Education Level"
  }
  if(category == "age") {
    level = c(c("15-24","25-44", "45-54","55+"))
    title = "Age Group"
  }
  if(category == "lsm") {
    level = c("1-2", "3-4", "5-6", "7-8", "9-10")
    title = "LSM"
  }
  if(category == "sex") {
    level = c("male", "female")
    title = "Gender"
  }
  if(category == "hh_inc") {
    level = c("<5000","5000-10999","11000-19999",">=20000")
    title = "Household Income"
  }
  
  ggplot(data = set10c, aes_string(x = "cluster", fill = category)) +
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_discrete(labels=level) +
    labs(title = title) +
    guides(fill=guide_legend(title=NULL)) +
    scale_x_discrete(labels = c("heavy","internet","medium","light"))
}

# to get percent of sample by cluster
table(pooled_set_c$cluster)/nrow(pooled_set_c)

# considering cluster by year...
cluster_by_year <- pooled_set_c %>%
  group_by(year) %>%
  count(cluster) %>%
  mutate(total = sum(n))

jpeg('cluster_by_year.jpeg')
ggplot(cluster_by_year) +
  aes(x = year, y = n, fill = cluster, label = paste0(round(100*(n/total)),"%") )+
  geom_bar(stat = 'identity') +
  geom_text(position = "stack", size = 3) +
  labs(y = "count")
dev.off()

# consider some linear models full models, all interactions with year
mod_radio <- lm(radio ~ 
                  age +
                  sex +
                  edu +
                  hh_inc +
                  race +
                  lsm +
                  year +
                  year * age +
                  year * sex +
                  year * edu +
                  year * hh_inc +
                  year * race +
                  year * lsm,
                data = pooled_set_c)
mod_tv <- lm(tv ~ 
                  age +
                  sex +
                  edu +
                  hh_inc +
                  race +
                  lsm +
                  year +
                  year * age +
                  year * sex +
                  year * edu +
                  year * hh_inc +
                  year * race +
                  year * lsm,
                data = pooled_set_c)
mod_newspapers <- lm(newspapers ~ 
                  age +
                  sex +
                  edu +
                  hh_inc +
                  race +
                  lsm +
                  year +
                  year * age +
                  year * sex +
                  year * edu +
                  year * hh_inc +
                  year * race +
                  year * lsm,
                data = pooled_set_c)
mod_magazines <- lm(magazines ~ 
                      age +
                      sex +
                      edu +
                      hh_inc +
                      race +
                      lsm +
                      year +
                      year * age +
                      year * sex +
                      year * edu +
                      year * hh_inc +
                      year * race +
                      year * lsm,
                    data = pooled_set_c)
mod_internet <- lm(internet ~ 
                      age +
                      sex +
                      edu +
                      hh_inc +
                      race +
                      lsm +
                      year +
                      year * age +
                      year * sex +
                      year * edu +
                      year * hh_inc +
                      year * race +
                      year * lsm,
                    data = pooled_set_c)


# setting up marginal means objects

# creating dataset for plotting
# function:
fr_set <- function(model, spec) {
  
  # link marginal means package
  require(emmeans)
  
  # create emmeans object
  temp1 <- emmeans(model, specs = spec, by = "year")
  
  # create subsettable summary object
  temp2 <- summary(temp1)
  
  # create output set
  cbind.data.frame(category = temp2[[1]],
                   year = temp2[[2]],
                   means = temp2[[3]],
                   lower = temp2[[6]],
                   upper = temp2[[7]])
  
  
}

# joining into single frame
pool_means_radio <- rbind(fr_set(mod_radio, spec = "sex"),
                          fr_set(mod_radio, spec = "age"),
                          fr_set(mod_radio, spec = "race"),
                          fr_set(mod_radio, spec = "edu"),
                          fr_set(mod_radio, spec = "hh_inc"),
                          fr_set(mod_radio, spec = "lsm"))
pool_means_tv <- rbind(fr_set(mod_tv, spec = "sex"),
                          fr_set(mod_tv, spec = "age"),
                          fr_set(mod_tv, spec = "race"),
                          fr_set(mod_tv, spec = "edu"),
                          fr_set(mod_tv, spec = "hh_inc"),
                          fr_set(mod_tv, spec = "lsm"))
pool_means_magazines <- rbind(fr_set(mod_magazines, spec = "sex"),
                          fr_set(mod_magazines, spec = "age"),
                          fr_set(mod_magazines, spec = "race"),
                          fr_set(mod_magazines, spec = "edu"),
                          fr_set(mod_magazines, spec = "hh_inc"),
                          fr_set(mod_magazines, spec = "lsm"))
pool_means_newspapers <- rbind(fr_set(mod_newspapers, spec = "sex"),
                          fr_set(mod_newspapers, spec = "age"),
                          fr_set(mod_newspapers, spec = "race"),
                          fr_set(mod_newspapers, spec = "edu"),
                          fr_set(mod_newspapers, spec = "hh_inc"),
                          fr_set(mod_newspapers, spec = "lsm"))
pool_means_internet <- rbind(fr_set(mod_internet, spec = "sex"),
                          fr_set(mod_internet, spec = "age"),
                          fr_set(mod_internet, spec = "race"),
                          fr_set(mod_internet, spec = "edu"),
                          fr_set(mod_internet, spec = "hh_inc"),
                          fr_set(mod_internet, spec = "lsm"))

save(pool_means_internet,
     pool_means_magazines,
     pool_means_newspapers,
     pool_means_tv,
     pool_means_radio, file = "pool_means.RData")

load("pool_means.RData")

# doing some plots:
vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")

# function for plotting fitted models
plot_fitted <- function(data, medium) { # medium: one of: newspapers, magazines, radio, tv, internet
  
  if(medium == "newspapers") {
    a <- "means"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "Newspapers"
  }
  if(medium == "magazines") {
    a <- "means"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "Magazines"
  }
  if(medium == "tv") {
    a <- "means"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "TV"
  }
  if(medium == "radio") {
    a <- "means"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "Radio"
  }
  if(medium == "internet") {
    a <- "means"
    c <- "upper"
    d <- "lower"
    e <- "engagement"
    f <- "Internet"
  }
  
  #plot
  ggplot(data = data, aes_string("year", a, group = "category")) +
    geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
    geom_line(size = 0.2) +
    facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
    geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
    labs(y = e, title = f) +
    coord_cartesian(ylim=c(-2, 0.8)) + 
    scale_y_continuous(breaks=seq(-2, 0.8, 0.5))
  
}

## RADIO
pf_radio_up <- plot_fitted(data = pool_means_radio[which(pool_means_radio$category %in% vector_row1),],
                           medium = "radio")
pf_radio_down <- plot_fitted(data = pool_means_radio[which(pool_means_radio$category %in% vector_row2),],
                             medium = "radio")
jpeg("radio_pooled_means.jpeg", quality = 100)
grid.arrange(pf_radio_up, pf_radio_down, nrow = 2)
dev.off()

## NEWSPAPERS
pf_newspapers_up <- plot_fitted(data = pool_means_newspapers[which(pool_means_newspapers$category %in% vector_row1),],
                           medium = "newspapers")
pf_newspapers_down <- plot_fitted(data = pool_means_newspapers[which(pool_means_newspapers$category %in% vector_row2),],
                             medium = "newspapers")
jpeg("newspapers_pooled_means.jpeg", quality = 100)
grid.arrange(pf_newspapers_up, pf_newspapers_down, nrow = 2)
dev.off()

## MAGAZINES
pf_magazines_up <- plot_fitted(data = pool_means_magazines[which(pool_means_magazines$category %in% vector_row1),],
                                medium = "magazines")
pf_magazines_down <- plot_fitted(data = pool_means_magazines[which(pool_means_magazines$category %in% vector_row2),],
                                  medium = "magazines")
jpeg("magazines_pooled_means.jpeg", quality = 100)
grid.arrange(pf_magazines_up, pf_magazines_down, nrow = 2)
dev.off()

## TV
pf_tv_up <- plot_fitted(data = pool_means_tv[which(pool_means_tv$category %in% vector_row1),],
                                medium = "tv")
pf_tv_down <- plot_fitted(data = pool_means_tv[which(pool_means_tv$category %in% vector_row2),],
                                  medium = "tv")
jpeg("tv_pooled_means.jpeg", quality = 100)
grid.arrange(pf_tv_up, pf_tv_down, nrow = 2)
dev.off()

## INTERNET
pf_internet_up <- plot_fitted(data = pool_means_internet[which(pool_means_internet$category %in% vector_row1),],
                                medium = "internet")
pf_internet_down <- plot_fitted(data = pool_means_internet[which(pool_means_internet$category %in% vector_row2),],
                                  medium = "internet")
jpeg("internet_pooled_means.jpeg", quality = 100)
grid.arrange(pf_internet_up, pf_internet_down, nrow = 2)
dev.off()

