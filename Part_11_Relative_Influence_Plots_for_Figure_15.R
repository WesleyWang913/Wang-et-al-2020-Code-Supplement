Baseline.finfluence <- data.frame(
  Parameter= c("Intercept", "Te", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PIF Polygon Area", "PIF Ellipse Area", "PEF Polygon Area", "PEF Ellipse Area", "Ti Polygon Area", "Ti Ellipse Area",
               "Intercept", "Te", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PIF Polygon Area", "PIF Ellipse Area", "PEF Polygon Area", "PEF Ellipse Area", "Ti Polygon Area", "Ti Ellipse Area",
               "Intercept", "Te", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PIF Polygon Area", "PIF Ellipse Area", "PEF Polygon Area", "PEF Ellipse Area", "Ti Polygon Area", "Ti Ellipse Area"),
  Algorithm= c("RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF",
               "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM",
               "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV"),
  Influence= c(8.51,9.73,8.17,5.14,7.34,8.8,15.32,12.29,5.79,4.80,6.62,7.49,
               0,49.47,10.25,0.84,14.71,1.18,20.53,0.79,0.49,0.94,0.79,0,
               12.14,7.43,7.42,13.28,3.55,3.18,32.68,16.9,0.36,0.79,1.14,1.14)
)

Inf1 <- ggplot(data=Baseline.finfluence, aes(x=Parameter, y=Influence, fill= Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Baseline Frequency Feature Influence")+
  ylab("Relative Influence (%)")+
  ylim(0,50)

Baseline.TVinfluence <- data.frame(
  Parameter= c("Ti", "PIF", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PIF Polygon Area", "PIF Ellipse Area", "PEF Polygon Area", "PEF Ellipse Area", "Ti Polygon Area", "Ti Ellipse Area",
               "Ti", "PIF", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PIF Polygon Area", "PIF Ellipse Area", "PEF Polygon Area", "PEF Ellipse Area", "Ti Polygon Area", "Ti Ellipse Area",
               "Ti", "PIF", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PIF Polygon Area", "PIF Ellipse Area", "PEF Polygon Area", "PEF Ellipse Area", "Ti Polygon Area", "Ti Ellipse Area"),
  Algorithm= c("RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF", "RF",
               "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM", "GBM",
               "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV", "SV"),
  Influence= c(8.24,7.15,8.91,7.22,9.48,8.96,13.06,8.51,7.99,6.24,6.63,7.60,
               23.48,25.70,15.64,0.03,11.18,2.28,11.89,0.18,5.66,0.92,3.02,0,
               2.83,3.61,13.75,23.09,4.07,3.97,25.31,6.22,4.18,5.21,3.88,3.88))

Inf2<- ggplot(data=Baseline.TVinfluence, aes(x=Parameter, y=Influence, fill= Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))  +
  ggtitle("Baseline TV Feature Influence")  +
  ylab("Relative Influence (%)")+
  ylim(0,50)


Hypercapnia.finfluence <- data.frame(
  Parameter= c("R-Squared", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PEF Ellipse Area",
               "R-Squared", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PEF Ellipse Area",
               "R-Squared", "F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PEF Ellipse Area"),
  Algorithm= c("RF", "RF", "RF", "RF", "RF", "RF", 
               "GBM","GBM","GBM","GBM","GBM","GBM",
               "SV","SV","SV","SV","SV","SV"),
  Influence= c(31.45,27.41,11.86,6.55,6.14,16.59,
               82.87,9.38,2.01,5.08,0,0.67,
               20.83,16.72,12.02,19.11,22.64,8.67)
)

Inf3 <- ggplot(data=Hypercapnia.finfluence, aes(x=Parameter, y=Influence, fill= Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Hypercapnia Frequency Feature Influence")  +
  ylab("Relative Influence (%)")+
  ylim(0,50)


Hypercapnia.TVinfluence <- data.frame(
  Parameter= c("PIF", "Tr","F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PEF Ellipse Area", 
               "PIF", "Tr","F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PEF Ellipse Area", 
               "PIF", "Tr","F Polygon Area", "F Ellipse Area", "TV Polygon Area", "TV Ellipse Area", "PEF Ellipse Area"),
  Algorithm= c("RF", "RF", "RF", "RF", "RF", "RF", "RF",
               "GBM","GBM","GBM","GBM","GBM","GBM","GBM",
               "SV","SV","SV","SV","SV","SV","SV"),
  Influence= c(5.46,24.38,17.68,18.22,8.35,14.06,11.85,
               32.34,63.23,3.83,0,0.59,0,0,
               4.91,18.20,8.29,6.28,25.19,26.04,11.09)
)

Inf4 <- ggplot(data=Hypercapnia.TVinfluence, aes(x=Parameter, y=Influence, fill= Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))  +
  ggtitle("Hypercapnia TV Feature Influence")  +
  ylab("Relative Influence (%)")+
  ylim(0,50)


Baseline.AUC <- data.frame(
  Algorithm= c("RF","RF","GBM", "GBM","SVM","SVM"),
  AUC= c(0.875,0.944,0.7708,0.8333,0.8333,0.8889),
  Model=c("f", "TV","f", "TV","f", "TV")
)

Hyper.AUC <- data.frame(
  Algorithm= c("RF","RF","GBM", "GBM","SVM","SVM"),
  AUC= c(0.9,1,0.9,0.775,0.9,0.9),
  Model=c("f", "TV","f", "TV","f", "TV")
)

AUC1 <- ggplot(data=Baseline.AUC, aes(x=Model, y=AUC, fill=Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  ggtitle("Baseline Algorithms Area Under Curve")+
  ylim(0,1)

AUC2 <- ggplot(data=Hyper.AUC, aes(x=Model, y=AUC, fill=Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  ggtitle("Hypercapnia Algorithms Area Under Curve")  +
  ylim(0,1)



Baseline.Accuracy <- data.frame(
  Algorithm= c("RF","RF","GBM", "GBM","SVM","SVM"),
  Accuracy= c(75,90.91,66.67,72.73,91.67,81.82),
  Model=c("f", "TV","f", "TV","f", "TV")
)
Ac1 <- ggplot(data=Baseline.Accuracy, aes(x=Model, y=Accuracy, fill=Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  ggtitle("Baseline Confusion Matrix Accuracy") +
  ylim(0,100)+
  ylab("Accuracy (%)")



Hyper.Accuracy <- data.frame(
  Algorithm= c("RF","RF","GBM", "GBM","SVM","SVM"),
  Accuracy= c(87.5,100,87.5,77.78,87.5,88.89),
  Model=c("f", "TV","f", "TV","f", "TV")
)
Ac2 <- ggplot(data=Hyper.Accuracy, aes(x=Model, y=Accuracy, fill=Algorithm)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  ggtitle("Hypercapnia Confusion Matrix Accuracy") +
  ylim(0,100) +
  ylab("Accuracy (%)")




ggarrange(Ac1,Ac2,AUC1,AUC2, nrow = 2, ncol = 2, common.legend = TRUE, labels=c("A","B","C","D"))

ggarrange(Inf1,Inf2,Inf3,Inf4, nrow=2, ncol=2, legend = "none", labels=c("E","F","G","H"))

