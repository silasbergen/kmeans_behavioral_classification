library(dplyr)

eagle_subset <- read.csv('C:/Users/rt1875bv/Dropbox/Sabbatical/Eagle Article 1/ExampleCode/eagle_subset.csv')

segs <- eagle_subset %>% group_by(segment_id) %>% summarize(len = n())
##Pull off the relevant flight variables
##and look at histograms 

flight_vars <- eagle_subset %>% 
  select(KPH:abs_VR)

library(psych)
multi.hist(flight_vars, global = FALSE, breaks=40,density=FALSE)

##Sqrt transform the skewed ones and scale everything:

ready_to_cluster <- flight_vars %>% 
  mutate_at(vars(AGL, Sn, abs_VR,abs_angle), sqrt) %>% 
  scale()


##Ready to do K-means clustering.
##Will investigate for K in (2:7) as in paper
##10 random starting centroids each time
##increase iter.max to 30 to ensure algorithm has enough time to converge


#Set seed to ensure the same labels get applied to the clusters each time it's run.
#If not set, will get same convergence but the labels will be shuffled
#(due to different initial randomly allocated cluster membership)
set.seed(234) 
k2 <- kmeans(ready_to_cluster, centers = 2, nstart = 10, iter.max = 30)
set.seed(234) 
k3 <- kmeans(ready_to_cluster, centers = 3, nstart = 10, iter.max = 30)
set.seed(234) 
k4 <- kmeans(ready_to_cluster, centers = 4, nstart = 10, iter.max = 30)
set.seed(234) 
k5 <- kmeans(ready_to_cluster, centers = 5, nstart = 10, iter.max = 30)
set.seed(234) 
k6 <- kmeans(ready_to_cluster, centers = 6, nstart = 10, iter.max = 30)
set.seed(234) 
k7 <- kmeans(ready_to_cluster, centers = 7, nstart = 10, iter.max = 30)
#May throw warning message that it took many iterations to converge;
#should be okay as long as ifault = 0

k6$ifault
k7$ifault

##########################################################
##Next, we'll do some plotting to help us choose K.
## First, check out the within-sum-of-squares plot
##########################################################

wss_df = data.frame(k = 2:7, 
                    wss = c(k2$tot.withinss,
                            k3$tot.withinss,
                            k4$tot.withinss,
                            k5$tot.withinss,
                            k6$tot.withinss,
                            k7$tot.withinss))

#Looking for an elbow, in order to determine "best k":
with(wss_df, plot(k,wss,type='b'))
#no obvious elbow.  

##########################################################
##Next, we'll bootstrap the average silhouette width.
##Calculate the average silhouettes for B=100 bootstrap samples
## of size 1000 each (for computational ease)
## In paper, B = 1000 of size 10,000 each.
##########################################################

library(cluster)

B <- 100
avg.sils <- matrix(NA,B,6)
set.seed(992021)
for(b in 1:B){
  ids <- sample(1:nrow(ready_to_cluster),1000,replace=TRUE) 
  distmat <- dist(ready_to_cluster[ids,])
  clusters <- list(k2$cluster[ids],
                   k3$cluster[ids],
                   k4$cluster[ids],
                   k5$cluster[ids],
                   k6$cluster[ids],
                   k7$cluster[ids])
  for(i in 1:6) {
    sil <- silhouette(clusters[[i]],distmat) 
    avg.sils[b,i] <- mean(sil[,3])
  }
}

plot(2:7, avg.sils[1,],type='b',ylim=c(0.2, 0.35),col='#00000040', ylab='Average silhouette width',xlab = 'K')
for(i in 2:nrow(avg.sils)) lines(2:7, avg.sils[i,],type='b',col='#00000040')


##Let's add the cluster assignments to the data set

eagle_subset <- eagle_subset %>% 
  mutate(k2 = k2$cluster,
         k3 = k3$cluster,
         k4 = k4$cluster,
         k5 = k5$cluster,
         k6 = k6$cluster,
         k7 = k7$cluster)


##Time for a biplot lineup.
##First step: do PCA on the flight variables
##From there, create a lineup of biplots for each K, color coded by cluster


pca <- princomp(ready_to_cluster)

eagle_subset <- eagle_subset %>% 
  mutate(pc1 = pca$scores[,1],pc2 = pca$scores[,2])


library(ggplot2)

#Below is a color-blind palette, per 
# https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

cbpal <- c("#009E73","#F0E442", "#000000",  "#D55E00","#56B4E9","#999999","#CC79A7") #black instead of green; harsh yellow; red not pink


#K=2
ggplot(data = eagle_subset) + 
  geom_point(aes(x = pc1, y = pc2, col = factor(k2)),shape='.')+
  xlim(c(-5,5)) + ylim(c(-6,6)) + #Just to zoom in on the biplot a bit
  guides(color='none') + 
  xlab('PC1') + ylab('PC2')+ 
  ggtitle('K=2') +
  scale_color_manual(values = cbpal[2:1])


#K=3
ggplot(data = eagle_subset) + 
  geom_point(aes(x = pc1, y = pc2, col = factor(k3)),shape='.')+
  xlim(c(-5,5)) + ylim(c(-6,6)) +
  guides(color='none') + 
  xlab('PC1') + ylab('PC2')+ 
  ggtitle('K=3') +
  scale_color_manual(values = cbpal[c(1,2,5)])


#K=4
ggplot(data = eagle_subset) + 
  geom_point(aes(x = pc1, y = pc2, col = factor(k4)),shape='.')+
  xlim(c(-5,5)) + ylim(c(-6,6)) +
  guides(color='none') + 
  xlab('PC1') + ylab('PC2')+ 
  ggtitle('K=4') +
  scale_color_manual(values = cbpal[c(5,4,1,2)])



#...etc



####################################################
##Next, let's examine the relationships of the 
## flight variables with the K = 5 clusters
## Remove outliers for faster plotting with lots of data
####################################################

##Note that the cluster labels are different from those in the paper,
##due to different starting points in the kmeans as the data
## here is just a subset of the data in the paper.
##however the same behavioral characteristics 
##(perching, flapping, ascending, directional, gliding) are still readily identified.

##For consistency will define behaviors and reorder to be same as paper

eagle_subset <- eagle_subset %>% 
  mutate(behavior = case_when(
                              k4==1~'gliding',
                              k4==2~'flapping',
                              k4==3~'perching',
                              k4==4~'ascending')) %>% 
  mutate(behavior = factor(behavior, levels = c('perching','ascending','flapping','gliding')))

pdf('boxplots.pdf')
ggplot(data = eagle_subset) + 
  geom_boxplot(aes(x = behavior, fill = behavior, y = AGL),outlier.shape = NA) + 
  scale_fill_manual(values = cbpal[c(1,2,4,5)])
ggplot(data = eagle_subset) + 
  geom_boxplot(aes(x = behavior, fill = behavior, y = abs_angle),outlier.shape = NA) + 
  scale_fill_manual(values =cbpal[c(1,2,4,5)])
ggplot(data = eagle_subset) + 
  geom_boxplot(aes(x = behavior, fill = behavior, y = KPH),outlier.shape = NA) + 
  scale_fill_manual(values = cbpal[c(1,2,4,5)])
ggplot(data = eagle_subset) + 
  geom_boxplot(aes(x = behavior, fill = behavior, y = VerticalRate),outlier.shape = NA) + 
  scale_fill_manual(values = cbpal[c(1,2,4,5)]) + ylim(c(-5,5)) + 
  geom_hline(aes(yintercept=0), linetype=2)
dev.off()

####################################################
##Finally, let's plot a flight path.
####################################################


#Find a relatively long one,
#but not so long that it's hard to see the colors

segment_lengths <- eagle_subset %>% 
  group_by(segment_id) %>% 
  summarize(len = n()) %>% 
  arrange(-len) 

head(segment_lengths,20)

segment_to_plot <- eagle_subset %>% 
  filter(segment_id==7471)

ggplot(data = segment_to_plot,aes(x = X, y = Y)) +
  geom_point(aes(col = behavior),size=.5) + 
  scale_color_manual(values = cbpal[c(1,2,4,5)])
