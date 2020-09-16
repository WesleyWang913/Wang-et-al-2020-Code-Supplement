#Pool Data for Corplots
#-----
#P1 Control Baseline
P1Control.Pool <- rbind(P1DataControl1, P1DataControl2,P1DataControl3, P1DataControl4,P1DataControl5, P1DataControl6,P1DataControl7, P1DataControl8, P1DataControl9)
colnames(P1Control.Pool) <- c("f", "TV", "PIF", "PEF", "Ti", "Te", "Tr")
P1ControlCor <- cor(P1Control.Pool)
P1Control.corplot <- ggcorrplot(P1ControlCor, title = "P1 Baseline")

#P21 Control Baseline
P21Control.Pool <- rbind(P21DataControl1, P21DataControl2,P21DataControl3, P21DataControl4,P21DataControl5, P21DataControl6,P21DataControl7, P21DataControl8)
colnames(P21Control.Pool) <- c("f", "TV", "PIF", "PEF", "Ti", "Te", "Tr")
P21ControlCor <- cor(P21Control.Pool)
P21Control.corplot <- ggcorrplot(P21ControlCor, title = "P21 Baseline")

#P21 Control Hypercapnic
P21ControlHyper.Pool <- rbind(P21DataControlHyper1, P21DataControlHyper2,P21DataControlHyper3, P21DataControlHyper4,P21DataControlHyper5, P21DataControlHyper6,P21DataControlHyper7, P21DataControlHyper8)
colnames(P21ControlHyper.Pool) <- c("f", "TV", "PIF", "PEF", "Ti", "Te", "Tr")
P21ControlHyperCor <- cor(P21ControlHyper.Pool)
P21Hyper.corplot <- ggcorrplot(P21ControlHyperCor, title = "P21 Hypercapnia")

#P56 Control Baseline
P56Control.Pool <- rbind(P56DataControl1, P56DataControl2,P56DataControl3, P56DataControl4,P56DataControl5, P56DataControl6,P56DataControl7, P56DataControl8, P56DataControl9)
colnames(P56Control.Pool) <- c("f", "TV", "PIF", "PEF", "Ti", "Te", "Tr")
P56ControlCor <- cor(P56Control.Pool)
P56Control.corplot <- ggcorrplot(P56ControlCor, title = "P56 Baseline")

#P56 Control Hypercapnic
P56ControlHyper.Pool <- rbind(P56DataControlHyper1, P56DataControlHyper2,P56DataControlHyper3, P56DataControlHyper4,P56DataControlHyper5, P56DataControlHyper6,P56DataControlHyper7, P56DataControlHyper8, P56DataControlHyper9)
colnames(P56ControlHyper.Pool) <- c("f", "TV", "PIF", "PEF", "Ti", "Te", "Tr")
P56ControlHyperCor <- cor(P56ControlHyper.Pool)
P56Hyper.corplot <- ggcorrplot(P56ControlHyperCor, title = "P56 Hypercapnia")

ggarrange(P1Control.corplot, P21Control.corplot, P56Control.corplot, 
          blank, P21Hyper.corplot, P56Hyper.corplot,
          labels = c("A","B","C", "","D","E"),
          ncol = 3, nrow = 2, common.legend = TRUE)
