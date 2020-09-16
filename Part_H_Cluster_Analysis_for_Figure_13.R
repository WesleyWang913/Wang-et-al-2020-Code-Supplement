library(cluster)
library(factoextra)
#Kmeans cluster analysis of LDA plots
set.seed(20)
gap_stat <- clusGap(ControlBase.fplot.data[,1:2], kmeans, nstart=25, K.max=10, B=50)
Gap1 <- fviz_gap_stat(gap_stat) +
  ggtitle("Baseline Frequency K-Cluster Permutation")
Basef.cluster <- kmeans(ControlBase.fplot.data[,1:2], 3, nstart=25)
Basef.cluster$cluster <- as.factor(Basef.cluster$cluster)
Cluster1 <- ggplot(ControlBase.fplot.data, aes(X, Y, color = Basef.cluster$cluster)) +
  geom_point() +
  stat_ellipse(type="norm", geom = "polygon", alpha=0.2, aes(fill=Basef.cluster$cluster))+
  ggtitle("Baseline Frequency Cluster Analysis") +
  xlab("LD1") +
  ylab("LD2")  + 
  labs(color = "Kmeans Clusters", fill= "Kmeans Clusters") +
  scale_color_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)" )) +
  scale_fill_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)") ) 

gap_stat <- clusGap(ControlBase.TVplot.data[,1:2], kmeans, nstart=25, K.max=10, B=50)
Gap2 <- fviz_gap_stat(gap_stat)+
  ggtitle("Baseline Tidal Volume K-Cluster Permutation")
BaseTV.cluster <- kmeans(ControlBase.TVplot.data[,1:2], 3, nstart=25)
BaseTV.cluster$cluster <- as.factor(BaseTV.cluster$cluster)
Cluster2 <- ggplot(ControlBase.TVplot.data, aes(X, Y, color = BaseTV.cluster$cluster)) +
  geom_point() +
  stat_ellipse(type="norm", geom = "polygon", alpha=0.2, aes(fill=BaseTV.cluster$cluster))+
  ggtitle("Baseline Tidal Volume Cluster Analysis") +
  xlab("LD1") +
  ylab("LD2") +
  theme_bw() + 
  labs(color = "Kmeans Clusters", fill= "Kmeans Clusters") +
  scale_color_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)" )) +
  scale_fill_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)") ) 


ControlHyper.fplot.gap <- cbind(ControlHyper.fplot.data[,1], 0)
gap_stat <- clusGap(ControlHyper.fplot.gap, kmeans, nstart=25, K.max=10, B=50)
Gap3 <- fviz_gap_stat(gap_stat)+
  ggtitle("Hypercapnia Frequency K-Cluster Permutation")
Hyperf.cluster <- kmeans(ControlHyper.fplot.gap, 2, nstart =20)
Hyperf.cluster$cluster <- as.factor(Hyperf.cluster$cluster)
Cluster3 <- ggplot(ControlHyper.fplot.data, aes(X, y=0, color = Hyperf.cluster$cluster)) +
  geom_point() +
  ggtitle("Hypercapnic Frequency Cluster Analysis") +
  xlab("LD1") +
  ylab("LD2") +
  theme_bw()+ 
  labs(color = "Kmeans Clusters", fill= "Kmeans Clusters") +
  scale_color_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)" )) +
  scale_fill_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)") ) 


ControlHyper.TVplot.gap <- cbind(ControlHyper.TVplot.data[,1], 0)
gap_stat <- clusGap(ControlHyper.TVplot.gap, kmeans, nstart=25, K.max=10, B=50)
Gap4 <- fviz_gap_stat(gap_stat)+
  ggtitle("Hypercapnia Tidal Volume K-Cluster Permutation")
HyperTV.cluster <- kmeans(ControlHyper.TVplot.data[,1], 2, nstart =20)
HyperTV.cluster$cluster <- as.factor(HyperTV.cluster$cluster)
Cluster4 <- ggplot(ControlHyper.TVplot.data, aes(X, y=0, color = HyperTV.cluster$cluster)) +
  geom_point() +
  ggtitle("Hypercapnic Tidal Volume Cluster Analysis") +
  xlab("LD1") +
  ylab("LD2")+
  theme_bw() + 
  labs(color = "Kmeans Clusters", fill= "Kmeans Clusters") +
  scale_color_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)" )) +
  scale_fill_manual(values=c("orange", "purple", "gold"), labels= c("Cluster 3 (100 % composed of ground truth label P56)", "Cluster 2 (100 % composed of ground truth label P21)", "Cluster 1 (100 % composed of ground truth label P1)") ) 



ggarrange(Cluster1, Gap1,
          Cluster2, Gap2,
          Cluster3, Gap3,
          Cluster4, Gap4,
          nrow = 4, ncol = 2, common.legend = TRUE,
          labels = c("A","E","B","F","C","G","D","H"))

