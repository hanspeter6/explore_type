## code to process type analysis

# load packages
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(gridExtra)
library(caret)
library(randomForest)
library(MASS)
library(tidyr)
library(emmeans)
library(formula.tools)

# simple_print
set02_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2002/set02_simple_print.rds")
# set05_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2005/set05_simple_print.rds")
set08_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2008/set08_simple_print.rds")
set10_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2010/set10_simple_print.rds")
set12_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2012/set12_simple_print.rds")
set14_simple_print <- readRDS("/Users/hans-peterbakker/Dropbox/Statistics/UCTDataScience/Thesis/amps_2014/set14_simple_print.rds")


# function to add year as variable and get rid of vehicles
add_year <- function(set, year) {
  set %>%
    mutate(year = year) %>%
    dplyr::select(qn, pwgt, year, age, sex, edu, hh_inc, race, lsm, newspapers, magazines, radio, tv, internet, all)
}

# combine into single dataset:
# -> excluding 'lifestages' (since NA in 2008),
#  -> excluding 'lifestyle' and 'attitudes' (since missing in 2002); 
#  -> already excluded 2005 since has only one value for internet engagement
set_simple_print <- rbind.data.frame(add_year(set02_simple_print[,!names(set02_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2002),
                                         add_year(set08_simple_print[,!names(set08_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2008),
                                         add_year(set10_simple_print[,!names(set10_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2010),
                                         add_year(set12_simple_print[,!names(set12_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2012),
                                         add_year(set14_simple_print[,!names(set14_simple_print) %in% c("lifestages", "lifestyle", "attitudes")], 2014))
## Correlations
# function to use for plotting
corrPlot <- function(set, title) {
  ggcorrplot(cor(set),
             hc.order = FALSE,
             method = "square",
             type = "full",
             lab = TRUE,
             title = title,
             colors = c("snow1", "skyblue1", "royalblue"),
             legend.title = "",
             ggtheme = ggplot2::theme_grey,
             lab_size = 5,
             tl.cex = 14,
             show.legend = FALSE) +
    theme(plot.title = element_text(size = 18))
  
}

# running the plotting function
c02 <- corrPlot(set02_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                title = "2002")
c08 <- corrPlot(set08_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                title = "2008")
c10 <- corrPlot(set10_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                title = "2010")
c12 <- corrPlot(set12_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                title = "2012")
c14 <- corrPlot(set14_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                title = "2014")
c_all <- corrPlot(set_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                title = "Full Dataset")

# print to file for publication
pdf(file = "corrplots1.pdf", width = 17, height = 12, family = "Helvetica") # defaults to 7 x 7 inches
grid.arrange(c02,
             c08,
             c10,
             c12,
             c14,
             c_all,
             ncol = 3, nrow = 2)
dev.off()


# standardising the media type variables (including "all")
set_simple_print_std <- cbind.data.frame(set_simple_print[,1:9], scale(set_simple_print[,c("newspapers","magazines","radio", "tv", "internet", "all")]))

### Clustering

## consider how many centers to use
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6,7,8,9,10)) {
  temp <- kmeans(set_simple_print_std[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                 centers = k,
                 nstart = 5,
                 iter.max = 30)
  wss <- append(wss,temp$tot.withinss)
}

# plotting screeplot for myself
plot(c(1,2,3,4,5,6,7,8,9,10), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )

# run kmeans on 4 clusters
set.seed(123)
kmeans_pooled <- kmeans(set_simple_print_std[,c("newspapers","magazines","radio", "tv", "internet","all")],
                        centers = 4,
                        nstart = 5,
                        iter.max = 100)

# check cluster balances
table(kmeans_pooled$cluster)

# add cluster variable to pooled set...
set_simple_print_std_c <- set_simple_print_std %>%
  mutate(cluster = factor(kmeans_pooled$cluster)) %>%
  dplyr::select(qn, cluster, everything())

# rename levels for cluster to keep aligned with write-up:
set_simple_print_std_c$cluster <- ifelse(set_simple_print_std_c$cluster == "1", "7", set_simple_print_std_c$cluster)
set_simple_print_std_c$cluster <- ifelse(set_simple_print_std_c$cluster == "2", "9", set_simple_print_std_c$cluster)
set_simple_print_std_c$cluster <- ifelse(set_simple_print_std_c$cluster == "3", "8", set_simple_print_std_c$cluster)
set_simple_print_std_c$cluster <- ifelse(set_simple_print_std_c$cluster == "4", "6", set_simple_print_std_c$cluster)
set_simple_print_std_c$cluster <- as.factor(as.numeric(set_simple_print_std_c$cluster) - 5)

# set factor labels
set_simple_print_std_c$age <- factor(set_simple_print_std_c$age, labels = c("15-24","25-44", "45-54","55+"), ordered = FALSE)
set_simple_print_std_c$race <- factor(set_simple_print_std_c$race,labels = c("black", "coloured", "indian", "white"), ordered = FALSE)
set_simple_print_std_c$edu <- factor(set_simple_print_std_c$edu, labels = c("<matric", "matric",">matric" ) ,ordered = FALSE)
set_simple_print_std_c$lsm <- factor(set_simple_print_std_c$lsm, labels = c("LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10"), ordered = FALSE)
set_simple_print_std_c$sex <- factor(set_simple_print_std_c$sex, labels = c("male", "female"), ordered = FALSE)
set_simple_print_std_c$hh_inc <- factor(set_simple_print_std_c$hh_inc, labels = c("<R5000","R5000-R10999","R11000-R19999","R20000+"), ordered = FALSE)
set_simple_print_std_c$cluster <- factor(set_simple_print_std_c$cluster,  labels = c("heavy", "internet", "medium", "light"), ordered = FALSE)
set_simple_print_std_c$year <- factor(set_simple_print_std_c$year, ordered = FALSE)

# save it
saveRDS(set_simple_print_std_c, "set_simple_print_std_c.rds")

# read back
set_simple_print_std_c <- readRDS("set_simple_print_std_c.rds")

## multi-dimensional scaling

# # 1st create a subset of 10 000 cases to ensure easier running
# set.seed(56)
# sub_pooled <- set_simple_print_std_c[sample(nrow(set_simple_print_std_c), size = 10000),]
# 
# # distance matrix and MDS
# sub_pooled_dist <- dist(sub_pooled[,c("newspapers","magazines","radio", "tv", "internet", "all")])
# 
# # doing the multidimensional scaling on the sample set of 10 000 cases
# mds_pooled <- cmdscale(sub_pooled_dist)
# 
# # save the MDS object to avoid running every time.
# saveRDS(mds_pooled, "mds_pooled.rds")

# read back MDS ojbect (takes time to run...)
mds_pooled <- readRDS("mds_pooled.rds")

# plot for scree
screedata <- cbind.data.frame(k = c(1,2,3,4,5,6,7,8,9,10), wss = wss)
screeplot <- ggplot(screedata, aes(x = k, y = wss)) +
  geom_line() + geom_point() +
  scale_x_continuous("k = number of clusters", labels = as.character(c(1,2,3,4,5,6,7,8,9,10)), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  labs(y = "total within sum of squares", title = "Within Sum of Squares for Different Values of k" ) +
  theme(plot.title = element_text(size = 24),
        text = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12))

# plot for clusters
mdsForggplot <- cbind.data.frame(mds_pooled, col = as.factor((as.numeric(sub_pooled$cluster))))
names(mdsForggplot) <- c("x", "y", "col")
mds_plot <- ggplot(mdsForggplot, aes(x = x, y = y, col = col)) +
  geom_point(shape = 16, size = 5, alpha = .5) +
  labs(title = "Multidimensional Scaling of 4 Clusters:10 000 cases") +
  scale_colour_discrete(labels = c("heavy", "internet", "medium", "light")) +
  theme(
    legend.title = element_blank(),
        plot.title = element_text(size = 24),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(),
        text = element_text(size = 18),
        axis.text.x = element_text(size = 12))

# send both to file for publication
pdf(file = "scree_and_clusters.pdf", width = 20, height = 10, family = "Helvetica") # defaults to 7 x 7 inches
grid.arrange(screeplot,mds_plot,
             nrow = 1, ncol = 2
             )
dev.off()

# predictions to test clusters...
# create training and test sets:
set.seed(56)
ind_train <- createDataPartition(set_simple_print_std_c$cluster, p = 0.7, list = FALSE)
training <- set_simple_print_std_c[ind_train,]
testing <- set_simple_print_std_c[-ind_train,]

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
    labs(title = type) +
    theme(
      axis.title = element_blank(),
      plot.title = element_text(size = 20),
      axis.text.x = element_text(size = 16)
    )
  
}

# generate grid plot for publication
pdf(file = 'typeBoxPlots_pooled.pdf', width = 15, height = 15, family = "Helvetica")
grid.arrange(boxplot_clusters(set_simple_print_std_c, type = "all"),
             boxplot_clusters(set_simple_print_std_c, type = "newspapers"),
             boxplot_clusters(set_simple_print_std_c, type = "magazines"),
             boxplot_clusters(set_simple_print_std_c, type = "radio"),
             boxplot_clusters(set_simple_print_std_c, type = "tv"),
             boxplot_clusters(set_simple_print_std_c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

## make sense of demographics

# size of each cluster
ggplot(data = set_simple_print_std_c, aes(x = cluster, fill = cluster)) +
  geom_bar(stat = "count") +
  guides(fill = FALSE) 

## demographics by cluster

# function to draw bars by cluster
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
    title = "Living Standards Measure"
  }
  if(category == "sex") {
    level = c("male", "female")
    title = "Gender"
  }
  if(category == "hh_inc") {
    level = c("<R5000","R5000-R10999","R11000-R19999","R20000+")
    title = "Household Income"
  }
  
  ggplot(data = set, aes_string(x = "cluster", fill = category)) +
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_discrete(labels=level) +
    labs(title = title) +
    guides(fill=guide_legend(title=NULL)) +
    scale_x_discrete(labels = c("heavy","internet","medium","light")) +
    theme(
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.5,"cm"),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 20)
      )
}

# send grid of plots to file for publication
pdf(file = 'typeDemog_pooled2.pdf', width = 15, height = 15, family = "Helvetica")
grid.arrange(bars_by_cluster(set_simple_print_std_c, "sex"),
             bars_by_cluster(set_simple_print_std_c,"age"),
             bars_by_cluster(set_simple_print_std_c,"race"),
             bars_by_cluster(set_simple_print_std_c,"edu"),
             bars_by_cluster(set_simple_print_std_c,"hh_inc"),
             bars_by_cluster(set_simple_print_std_c,"lsm"), nrow = 3, ncol = 2)
dev.off()


jpeg("typeDemog_pooled2.jpeg")
grid.arrange(bars_by_cluster(set_simple_print_std_c, "sex"),
             bars_by_cluster(set_simple_print_std_c,"age"),
             bars_by_cluster(set_simple_print_std_c,"race"),
             bars_by_cluster(set_simple_print_std_c,"edu"),
             bars_by_cluster(set_simple_print_std_c,"hh_inc"),
             bars_by_cluster(set_simple_print_std_c,"lsm"), nrow = 3, ncol = 2)
dev.off()
             
# to get percent of sample by cluster
table(set_simple_print_std_c$cluster)/nrow(set_simple_print_std_c)

# considering cluster by year...
cluster_by_year <- set_simple_print_std_c %>%
  group_by(year) %>%
  count(cluster) %>%
  mutate(total = sum(n))

jpeg('cluster_by_year.jpeg')
ggplot(cluster_by_year) +
  aes(x = year, y = n, fill = cluster, label = paste0(round(100*(n/total)),"%") )+
  geom_bar(stat = 'identity') +
  geom_text(position = position_stack(vjust = 0.5), size = 4) +
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
                data = set_simple_print_std_c)




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
             data = set_simple_print_std_c)

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
                     data = set_simple_print_std_c)
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
                    data = set_simple_print_std_c)
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
                   data = set_simple_print_std_c)




# trying to figure out why lsm equal and prop so close but otherwise not...(same with freeTV below)

# create a database of proportions by category levels, ie also by year:
library(formula.tools)
proportions <- data.frame()
for(i in c("age","sex", "edu", "hh_inc", "race", "lsm")) {
  
  temp1 <- as.data.frame(prop.table(xtabs(as.formula(paste0(as.character(~ year ) , "+ ", i)), data = set_simple_print_std_c), margin = 1))
  
  proportions <- rbind.data.frame(proportions, temp1 %>%
                                    mutate(category = (paste0(i, ".", temp1[,2]))) %>%
                                    dplyr::select(year, category, proportion = Freq))
}

library(stringr)
equal <- rep(0, nrow(proportions))
equal[which(str_detect(proportions$category, "age|hh_inc|race"))] <- 1/4
equal[which(str_detect(proportions$category, "sex"))] <- 1/2
equal[which(str_detect(proportions$category, "edu"))] <- 1/3
equal[which(str_detect(proportions$category, "lsm"))] <- 1/5

strproportions$category
# add column of equal proportions
prop_equal <- proportions %>%
  mutate(equal = equal)

View(sort(coef(mod_tv)))
View(sort(coef(mod_radio)))
View(sort(coef(mod_internet)))

# setting up marginal means objects

# creating dataset for plotting
# # function:
# fr_set_old <- function(model, spec) {
#   
#   # link marginal means package
#   require(emmeans)
#   
#   # create emmeans object
#   temp1 <- emmeans(model, specs = spec, by = "year")
#   
#   # create subsettable summary object
#   temp2 <- summary(temp1)
#   
#   # create output set
#   cbind.data.frame(category = temp2[[1]],
#                    year = temp2[[2]],
#                    mean = temp2[[3]],
#                    upper = temp2[[7]],
#                    lower = temp2[[6]])
#   
#   
# }


fr_set <- function(model, spec) {
  
  # link marginal means package
  require(emmeans)
  
  # create emmeans objects
  temp.equal <- emmeans(model, specs = spec, by = "year", weights = "equal")
  temp.proportional <- emmeans(model, specs = spec, by = "year", weights = "proportional")
  
  # create subsettable summary objects
  summary.equal <- summary(temp.equal)
  summary.proportional <- summary(temp.proportional)
 
   # create output set
  cbind.data.frame(category = summary.equal[[1]],
                   year = summary.equal[[2]],
                   proportional = summary.proportional[[3]],
                   equal = summary.equal[[3]],
                   equal.upper = summary.equal[[7]],
                   equal.lower = summary.equal[[6]],
                   prop.upper = summary.proportional[[7]],
                   prop.lower = summary.proportional[[6]]
                   )
  
}

# joining into single frames
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

# function for adding factors:
add_factor <- function(set, factor) {
  set %>%
    mutate(factor = factor)
}

# binding into one frame:

one_frame <- rbind(add_factor(pool_means_radio, "radio"),
                   add_factor(pool_means_internet, "internet"),
                   add_factor(pool_means_magazines, "magazines"),
                   add_factor(pool_means_tv, "tv"),
                   add_factor(pool_means_newspapers, "newspapers"))

one_frame2 <- gather(one_frame, key = weights, value = mean, proportional, equal)

one_frame3 <- one_frame2[,-c(3,4,5,6)]

types_set <- one_frame3 %>%
  mutate(upper = c(one_frame$prop.upper, one_frame$equal.upper)) %>%
  mutate(lower = c(one_frame$prop.lower, one_frame$equal.lower))

# trying all plots here.

# changing the dataset to consider only "proportional" and only "mean" values and then go wide with factor again
types_wide <- types_set %>%
  filter(weights == "proportional") %>%
  dplyr::select(category, year, factor, mean) %>%
  spread(key = factor, value = mean)

# defining a function


all_plots <- function(data, title = "All Media Types") {
  ggplot(data = data, title = title) +
    geom_line(aes(year, newspapers, group = category, colour = "newspaper")) +
    geom_line(aes(year, magazines, group = category, colour = "magazine")) +
    geom_line(aes(year, radio, group = category, colour = "radio")) +
    geom_line(aes(year, tv, group = category, colour = "tv")) +
    geom_line(aes(year, internet, group = category, colour = "internet")) +
    # geom_line(aes(year, alls, group = category, colour = "all")) +
    scale_colour_discrete(name="Media") +
    facet_grid(. ~ category) +
    theme(axis.text.x = element_text(size = 6)) +
    labs(y = "engagement", title = title)
  
}

vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
vector_row2 <- c("<matric", "matric",">matric", "<R5000","R5000-R10999","R11000-R19999","R20000+", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
p_up <- all_plots(types_wide[which(types_wide$category %in% vector_row1),])
p_down <- all_plots(types_wide[which(types_wide$category %in% vector_row2),])

all_plots(types_wide)

grid.arrange(p_up, p_down, nrow = 2)

# function for plotting fitted models
plot_type_wraps <- function(dataset, type) { # factor: one of...
  
  # making sure I have the packages
  require(tidyverse)
  require(gridExtra)
  require(ggplot2)
  
  # subset the data by factor
  factor_data <- dataset %>% filter(factor == type)
  
  # facet plot
  ggplot(data = factor_data, aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
    geom_line(size = 0.8) +
    facet_wrap(.~ category, ncol = 6) +
    geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.6, width = 0.2, alpha = 0.8) +
    theme(axis.text.x = element_text(size = 12, angle = 45 ),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 16, margin = ggplot2::margin(0,0,0,0, unit = "cm")),
          axis.title.y = element_text(size = 22),
          axis.title.x = element_text(size = 22),
          axis.ticks = element_line(size = 1),
          plot.margin = ggplot2::margin(0.5,0.5,0.5,0.5, unit = 'cm'),
          axis.ticks.length = unit(0.2, "cm"),
          panel.spacing = unit(5, 'pt'),
          panel.grid.minor = element_line(size = 0.5),
          panel.grid.major = element_line(size = 0.8),
          legend.position = c(0.85,0.07),
          legend.text = element_text(size = 16, margin = ggplot2::margin(0,0,0,0, unit = "cm")),
          legend.title = element_text(size = 20),
          legend.key.height = unit(2, "cm"),
          legend.key.size = unit(2, "cm")
    ) +
    scale_x_discrete(labels = c("'02","'08","'10","'12", "'14")) +
    coord_cartesian(ylim = c(-1, 1.2)) +
    labs(x = "years (2002 - 2014)", y = "engagement")
}

# send plots to files
pdf(file = "radio_emm.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
plot_type_wraps(types_set, "radio")
dev.off()

pdf(file = "newspapers_emm.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
plot_type_wraps(types_set, "newspapers")
dev.off()

pdf(file = "magazines_emm.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
plot_type_wraps(types_set, "magazines")
dev.off()

pdf(file = "tv_emm.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
plot_type_wraps(types_set, "tv")
dev.off()

pdf(file = "internet_emm.pdf", width = 12, height = 17, family = "Helvetica") # defaults to 7 x 7 inches
plot_type_wraps(types_set, "internet")
dev.off()


# 
# 
# # print plots to files
# jpeg("radio_emm.jpeg", res = 300)
# plot_types_wraps(types_set, "radio")
# dev.off()
# 
# jpeg("newspapers_emm.jpeg", res =300)
# plot_types_wraps(types_set, "newspapers")
# dev.off()
# 
# jpeg("magazines_emm.jpeg", res = 300)
# plot_types_wraps(types_set, "magazines")
# dev.off()
# 
# jpeg("tv_emm.jpeg", res = 300)
# plot_types_wraps(types_set, "tv")
# dev.off()
# 
# jpeg("internet_emm.jpeg", res = 300)
# plot_types_wraps(types_set, "internet")
# dev.off()

# PREVIOUS ATTEMPTS
# # function for plotting fitted models
# plot_types <- function(dataset, type) { # factor: one of...
#   
#   # making sure I have the packages
#   require(tidyverse)
#   require(gridExtra)
#   
#   # define upper middele and lower plots
#   row1 <- c("male", "female","15-24","25-44", "45-54","55+")
#   row2 <- c("black", "coloured", "indian", "white", "<matric", "matric",">matric")
#   row3 <- c( "<R5000","R5000-R10999","R11000-R19999","R20000+", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
#   
#   
#   # subset the data by factor
#   factor_data <- dataset %>% filter(factor == type)
#   
#   # row one plot
#   plot_row1 <- ggplot(data = factor_data[which(factor_data$category %in% row1),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
#     geom_line(size = 0.3) +
#     facet_grid(.~ category) +
#     geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.2, width = 0.3, alpha = 1) +
#     theme(axis.text.x = element_text(size = 3, angle = 45 ),
#           axis.title.x = element_blank(),
#           axis.text.y = element_text(size = 3),
#           strip.text.x = element_text(size = 4)) +
#     scale_x_discrete(labels = c("'02","'08","'10","'12", "'14")) +
#     coord_cartesian(ylim = c(-1, 1.2)) +
#     labs(y = "", size = 2, x = "") +
#     theme(legend.position = "bottom")
#   
#   # row two plot
#   plot_row2 <-  ggplot(data = factor_data[which(factor_data$category %in% row2),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
#     geom_line(size = 0.5) +
#     facet_grid(.~ category) +
#     geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.3, width = 0.3, alpha = 1) +
#     theme(axis.text.x = element_text(size = 6, angle = 45 ),
#           axis.text.y = element_text(size = 6)) +
#     scale_x_discrete(labels = c("'02","'08","'10","'12", "'14")) +
#     coord_cartesian(ylim = c(-1, 1.2)) +
#     labs(y = "engagement", size = 2, x = "")
#   
#   # row three plot
#   plot_row3 <-  ggplot(data = factor_data[which(factor_data$category %in% row3),], aes(x = year, y = mean, group = interaction(category,weights), col = weights)) +
#     geom_line(size = 0.5) +
#     facet_grid(.~ category) +
#     geom_errorbar(aes(ymax = upper, ymin = lower, colour = weights), size = 0.3, width = 0.3, alpha = 1) +
#     theme(axis.text.x = element_text(size = 6, angle = 45 ),
#           axis.text.y = element_text(size = 6),
#           strip.text.x = element_text(size = 8)) +
#     scale_x_discrete(labels = c("'02","'08","'10","'12", "'14")) +
#     coord_cartesian(ylim = c(-1, 1.2)) +
#     labs(y = "", size = 2)
#   
#   #extract legend
#   ##https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#   g_legend <- function(a.gplot) {
#     tmp <- ggplot_gtable(ggplot_build(a.gplot))
#     leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#     legend <- tmp$grobs[[leg]]
#     return(legend)}
#   
#   mylegend<-g_legend(plot_row1)
#   
#   grid.arrange(arrangeGrob(plot_row1 + theme(legend.position="none"),
#                            plot_row2 + theme(legend.position="none"),
#                            plot_row3 + theme(legend.position = "none")),
#                # top = paste0("Estimated Marginal Means: ", "'", type, "'"),
#                mylegend,
#                nrow=2,
#                heights=c(10, 1))
#   
#   # coord_cartesian(ylim=c(-0.5, 0.5)) + 
#   # scale_y_continuous(breaks=seq(-0.5, 0.5, 0.2))
# }
# 
# 
# # doing some plots:
# vector_row1 <- c("male", "female","15-24","25-44", "45-54","55+","black", "coloured", "indian", "white")
# vector_row2 <- c("<matric", "matric",">matric", "<R2500","R2500-R6999","R7000-R11999",">=R12000", "LSM1-2", "LSM3-4", "LSM5-6", "LSM7-8", "LSM9-10")
# 
# # function for plotting fitted models
# plot_fitted <- function(data, medium) { # medium: one of: newspapers, magazines, radio, tv, internet
#   
#   if(medium == "newspapers") {
#     a <- "means"
#     c <- "upper"
#     d <- "lower"
#     e <- "engagement"
#     f <- "Newspapers"
#   }
#   if(medium == "magazines") {
#     a <- "means"
#     c <- "upper"
#     d <- "lower"
#     e <- "engagement"
#     f <- "Magazines"
#   }
#   if(medium == "tv") {
#     a <- "means"
#     c <- "upper"
#     d <- "lower"
#     e <- "engagement"
#     f <- "TV"
#   }
#   if(medium == "radio") {
#     a <- "means"
#     c <- "upper"
#     d <- "lower"
#     e <- "engagement"
#     f <- "Radio"
#   }
#   if(medium == "internet") {
#     a <- "means"
#     c <- "upper"
#     d <- "lower"
#     e <- "engagement"
#     f <- "Internet"
#   }
#   
#   #plot
#   ggplot(data = data, aes_string("year", a, group = "category")) +
#     geom_point(color = "blue", size = 1, fill = "white", alpha = 0.5) +
#     geom_line(size = 0.2) +
#     facet_grid(.~ category) + theme(axis.text.x = element_text(size = 6)) +
#     geom_errorbar(aes_string(ymax = c, ymin = d), size = 0.3, width = 0.4, alpha = 0.5) +
#     labs(y = e, title = f) +
#     coord_cartesian(ylim=c(-2, 0.8)) + 
#     scale_y_continuous(breaks=seq(-2, 0.8, 0.5))
#   
# }
# 
# ## RADIO
# pf_radio_up <- plot_fitted(data = pool_means_radio[which(pool_means_radio$category %in% vector_row1),],
#                            medium = "radio")
# pf_radio_down <- plot_fitted(data = pool_means_radio[which(pool_means_radio$category %in% vector_row2),],
#                              medium = "radio")
# jpeg("radio_pooled_means.jpeg", quality = 100)
# grid.arrange(pf_radio_up, pf_radio_down, nrow = 2)
# dev.off()
# 
# ## NEWSPAPERS
# pf_newspapers_up <- plot_fitted(data = pool_means_newspapers[which(pool_means_newspapers$category %in% vector_row1),],
#                                 medium = "newspapers")
# pf_newspapers_down <- plot_fitted(data = pool_means_newspapers[which(pool_means_newspapers$category %in% vector_row2),],
#                                   medium = "newspapers")
# jpeg("newspapers_pooled_means.jpeg", quality = 100)
# grid.arrange(pf_newspapers_up, pf_newspapers_down, nrow = 2)
# dev.off()
# 
# ## MAGAZINES
# pf_magazines_up <- plot_fitted(data = pool_means_magazines[which(pool_means_magazines$category %in% vector_row1),],
#                                medium = "magazines")
# pf_magazines_down <- plot_fitted(data = pool_means_magazines[which(pool_means_magazines$category %in% vector_row2),],
#                                  medium = "magazines")
# jpeg("magazines_pooled_means.jpeg", quality = 100)
# grid.arrange(pf_magazines_up, pf_magazines_down, nrow = 2)
# dev.off()
# 
# ## TV
# pf_tv_up <- plot_fitted(data = pool_means_tv[which(pool_means_tv$category %in% vector_row1),],
#                         medium = "tv")
# pf_tv_down <- plot_fitted(data = pool_means_tv[which(pool_means_tv$category %in% vector_row2),],
#                           medium = "tv")
# jpeg("tv_pooled_means.jpeg", quality = 100)
# grid.arrange(pf_tv_up, pf_tv_down, nrow = 2)
# dev.off()
# 
# ## INTERNET
# pf_internet_up <- plot_fitted(data = pool_means_internet[which(pool_means_internet$category %in% vector_row1),],
#                               medium = "internet")
# pf_internet_down <- plot_fitted(data = pool_means_internet[which(pool_means_internet$category %in% vector_row2),],
#                                 medium = "internet")
# jpeg("internet_pooled_means.jpeg", quality = 100)
# grid.arrange(pf_internet_up, pf_internet_down, nrow = 2)
# dev.off()
# 
