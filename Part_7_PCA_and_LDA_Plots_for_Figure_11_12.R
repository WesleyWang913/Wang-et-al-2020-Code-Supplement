#Make Compiled Frames
#-----
ControlBaseline.fframe <- rbind(P1Control.fframe,P21Control.fframe, P56Control.fframe)
ControlBaseline.fframe.age <- ControlBaseline.fframe$Age
ControlBaseline.fframe <- ControlBaseline.fframe[-9]
ControlBaseline.fframe <- as.data.frame(scale(ControlBaseline.fframe, scale = TRUE, center = TRUE))
ControlBaseline.fframe <- cbind(ControlBaseline.fframe, ControlBaseline.fframe.age)
names(ControlBaseline.fframe)[23] <- "Age"

t.test(P1Control.TVframe[,1],P21Control.TVframe[,1])
t.test(P1Control.TVframe[,1],P56Control.TVframe[,1])
t.test(P21Control.TVframe[,1],P56Control.TVframe[,1])

t.test(P21ControlHyper.TVframe[,1],P56ControlHyper.TVframe[,1])

ControlBaseline.TVframe <- rbind(P1Control.TVframe,P21Control.TVframe, P56Control.TVframe)
ControlBaseline.TVframe.age <- ControlBaseline.TVframe$Age
ControlBaseline.TVframe <- ControlBaseline.TVframe[-9]
ControlBaseline.TVframe <- as.data.frame(scale(ControlBaseline.TVframe, scale = TRUE, center = TRUE))
ControlBaseline.TVframe <- cbind(ControlBaseline.TVframe, ControlBaseline.TVframe.age)
names(ControlBaseline.TVframe)[23] <- "Age"

ControlHyper.fframe <- rbind(P21ControlHyper.fframe, P56ControlHyper.fframe)
ControlHyper.fframe.age <- ControlHyper.fframe$Age
ControlHyper.fframe <- ControlHyper.fframe[-9]
ControlHyper.fframe <- as.data.frame(scale(ControlHyper.fframe, scale = TRUE, center = TRUE))
ControlHyper.fframe <- cbind(ControlHyper.fframe, ControlHyper.fframe.age)
names(ControlHyper.fframe)[23] <- "Age"

ControlHyper.TVframe <- rbind(P21ControlHyper.TVframe, P56ControlHyper.TVframe)
ControlHyper.TVframe.age <- ControlHyper.TVframe$Age
ControlHyper.TVframe <- ControlHyper.TVframe[-9]
ControlHyper.TVframe <- as.data.frame(scale(ControlHyper.TVframe, scale = TRUE, center = TRUE))
ControlHyper.TVframe <- cbind(ControlHyper.TVframe, ControlHyper.TVframe.age)
names(ControlHyper.TVframe)[23] <- "Age"

colnames(ControlBaseline.fframe) <- c("Int", "PIF", "PEF", "Ti", "Te", "Tr", "R", "P", "f-PA", "f-EA", "TV-PA", "TV-EA", "PIF-PA", "PIF-EA", "PEF-PA", "PEF-EA", "Ti-PA", "Ti-EA", "Te-PA", "Te-EA", "Tr-PA", "Tr-EA", "Age")
colnames(ControlBaseline.TVframe) <- c("Int", "PIF", "PEF", "Ti", "Te", "Tr", "R", "P", "f-PA", "f-EA", "TV-PA", "TV-EA", "PIF-PA", "PIF-EA", "PEF-PA", "PEF-EA", "Ti-PA", "Ti-EA", "Te-PA", "Te-EA", "Tr-PA", "Tr-EA", "Age")
colnames(ControlHyper.fframe) <- c("Int", "PIF", "PEF", "Ti", "Te", "Tr", "R", "P", "f-PA", "f-EA", "TV-PA", "TV-EA", "PIF-PA", "PIF-EA", "PEF-PA", "PEF-EA", "Ti-PA", "Ti-EA", "Te-PA", "Te-EA", "Tr-PA", "Tr-EA", "Age")
colnames(ControlHyper.TVframe) <- c("Int", "PIF", "PEF", "Ti", "Te", "Tr", "R", "P", "f-PA", "f-EA", "TV-PA", "TV-EA", "PIF-PA", "PIF-EA", "PEF-PA", "PEF-EA", "Ti-PA", "Ti-EA", "Te-PA", "Te-EA", "Tr-PA", "Tr-EA", "Age")

#PCA and LDA Analysis
#-----
#Control Baseline
ControlBase.fpca <- prcomp(ControlBaseline.fframe[,-23], scale = T)
ControlBase.fpca.var <- ControlBase.fpca$sdev^2
ControlBase.fpca.var.per <- round(ControlBase.fpca.var/sum(ControlBase.fpca.var)*100, 1)
barplot(ControlBase.fpca.var.per, main="Control Baseline Frequency Scree Plot", xlab="Principal Component", ylab="Percent Variation")
ControlBase.fpca.data <- data.frame(Sample = ControlBaseline.fframe$Age,
                                    X=ControlBase.fpca$x[,1],
                                    Y=ControlBase.fpca$x[,2])
ControlBase.fres.pca <- ControlBase.fpca
fviz_eig(ControlBase.fres.pca)
PCA.1 <- fviz_pca_var(ControlBase.fres.pca,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title = "f Baseline PCA")


ControlBase.ftest <- lda(Age ~ ., data =ControlBaseline.fframe)
ControlBase.ftest_p <- predict(ControlBase.ftest)
ControlBase.fdata.lda.values <- ControlBase.ftest_p
ControlBase.fplot.data <- data.frame(X=ControlBase.fdata.lda.values$x[,1], Y=ControlBase.fdata.lda.values$x[,2], Diagnosis=ControlBaseline.fframe$Age)
ControlBase.ftest
LDA.1 <- ggplot(data=ControlBase.fplot.data, aes(x=X, y=Y)) +
  geom_point(colour = "white",shape = 21, size = 3) +
  aes(fill=Diagnosis) +
  scale_fill_manual(values=c("tomato", "palegreen4", "steelblue1")) +
  ggtitle("Baseline Frequency Measurements") +
  xlab("LD1 (98.25%)") +ylab("LD2 (1.75%)")+
  theme_bw()


ControlBase.TVpca <- prcomp(ControlBaseline.TVframe[,-23], scale = T)
ControlBase.TVpca.var <- ControlBase.TVpca$sdev^2
ControlBase.TVpca.var.per <- round(ControlBase.TVpca.var/sum(ControlBase.TVpca.var)*100, 1)
barplot(ControlBase.TVpca.var.per, main="Control Baseline Tidal Volume Scree Plot", xlab="Principal Component", ylab="Percent Variation")
ControlBase.TVpca.data <- data.frame(Sample = ControlBaseline.TVframe$Age,
                                     X=ControlBase.TVpca$x[,1],
                                     Y=ControlBase.TVpca$x[,2])
ControlBase.TVres.pca <- ControlBase.TVpca
fviz_eig(ControlBase.TVres.pca)
PCA.2 <- fviz_pca_var(ControlBase.TVres.pca,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title = "TV Baseline PCA")

ControlBase.TVtest <- lda(Age ~ ., data =ControlBaseline.TVframe)
ControlBase.TVtest_p <- predict(ControlBase.TVtest)
ControlBase.TVdata.lda.values <- ControlBase.TVtest_p
ControlBase.TVplot.data <- data.frame(X=ControlBase.TVdata.lda.values$x[,1], Y=ControlBase.TVdata.lda.values$x[,2], Diagnosis=ControlBaseline.TVframe$Age)
ControlBase.TVtest
LDA.2 <- ggplot(data=ControlBase.TVplot.data, aes(x=X, y=Y)) +
  geom_point(colour = "white",shape = 21, size = 3) +
  aes(fill=Diagnosis) +
  scale_fill_manual(values=c("tomato", "palegreen4", "steelblue1")) +
  ggtitle("Baseline Tidal Volume Measurements") +
  xlab("LD1 (98.69%)") +ylab("LD2 (1.31%)")+
  theme_bw()

#Control Hypercapnic
ControlHyper.fpca <- prcomp(ControlHyper.fframe[,-23], scale = T)
ControlHyper.fpca.var <- ControlHyper.fpca$sdev^2
ControlHyper.fpca.var.per <- round(ControlHyper.fpca.var/sum(ControlHyper.fpca.var)*100, 1)
barplot(ControlHyper.fpca.var.per, main="Control Hypercapnic Frequency Scree Plot", xlab="Principal Component", ylab="Percent Variation")
ControlHyper.fpca.data <- data.frame(Sample = ControlHyper.fframe$Age,
                                     X=ControlHyper.fpca$x[,1],
                                     Y=ControlHyper.fpca$x[,2])
ControlHyper.fres.pca <- ControlHyper.fpca
fviz_eig(ControlHyper.fres.pca)
PCA.3 <- fviz_pca_var(ControlHyper.fres.pca,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title= "f Hypercapnia PCA")


ControlHyper.ftest <- lda(Age ~ ., data =ControlHyper.fframe)
ControlHyper.ftest_p <- predict(ControlHyper.ftest)
ControlHyper.fdata.lda.values <- ControlHyper.ftest_p
ControlHyper.fplot.data <- data.frame(X=ControlHyper.fdata.lda.values$x[,1], Diagnosis=ControlHyper.fframe$Age)
ControlHyper.ftest
LDA.3 <- ggplot(data=ControlHyper.fplot.data, aes(x=X, y=0)) +
  geom_point(colour= "white", shape = 21, size = 3) +
  aes(fill=Diagnosis) +
  scale_fill_manual(values=c("palegreen4", "steelblue1")) +
  ggtitle("Hypercapnic Frequency Measurements") +
  xlab("LD1 (100%)") +ylab("LD2 (0%)")+
  theme_bw()

ControlHyper.TVpca <- prcomp(ControlHyper.TVframe[,-23], scale = T)
ControlHyper.TVpca.var <- ControlHyper.TVpca$sdev^2
ControlHyper.TVpca.var.per <- round(ControlHyper.TVpca.var/sum(ControlHyper.TVpca.var)*100, 1)
barplot(ControlHyper.TVpca.var.per, main="Control Hypercapnic Tidal Volume Scree Plot", xlab="Principal Component", ylab="Percent Variation")
ControlHyper.TVpca.data <- data.frame(Sample = ControlHyper.TVframe$Age,
                                      X=ControlHyper.TVpca$x[,1],
                                      Y=ControlHyper.TVpca$x[,2])
ControlHyper.TVres.pca <- ControlHyper.TVpca
fviz_eig(ControlHyper.TVres.pca)
PCA.4 <- fviz_pca_var(ControlHyper.TVres.pca,col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, title = "TV Hypercapnia PCA")

ControlHyper.TVtest <- lda(Age ~ ., data =ControlHyper.TVframe)
ControlHyper.TVtest_p <- predict(ControlHyper.TVtest)
ControlHyper.TVdata.lda.values <- ControlHyper.TVtest_p
ControlHyper.TVplot.data <- data.frame(X=ControlHyper.TVdata.lda.values$x[,1], Diagnosis=ControlHyper.TVframe$Age)
ControlHyper.TVtest
LDA.4 <- ggplot(data=ControlHyper.TVplot.data, aes(x=X, y=0)) +
  geom_point(colour = "white",shape = 21, size = 3) +
  aes(fill=Diagnosis) +
  scale_fill_manual(values=c("palegreen4", "steelblue1")) +
  ggtitle("Hypercapnic Tidal Volume Measurements") +
  xlab("LD1 (100%)") +ylab("LD2 (0%)")+
  theme_bw()

ggarrange(PCA.1, PCA.2,
          PCA.3, PCA.4,
          nrow=2, ncol=2, labels = c("A","B","C","D"), common.legend = TRUE)

ggarrange(ggarrange(LDA.1, LDA.2, labels = c("A","B"), common.legend = TRUE),
          ggarrange(LDA.3, LDA.4, labels = c("C","D"), common.legend = TRUE), nrow = 2, ncol=1)










