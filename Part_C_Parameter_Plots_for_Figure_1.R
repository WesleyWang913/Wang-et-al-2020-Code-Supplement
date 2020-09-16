P1Control.Pool[,8] <- "P1 Baseline"
P21Control.Pool[,8] <- "P21 Baseline"
P21ControlHyper.Pool[,8] <- "P21 Hypercapnia"
P56Control.Pool[,8] <- "P56 Baseline"
P56ControlHyper.Pool[,8] <- "P56 Hypercapnia"

Mice.Pool <- rbind(P1Control.Pool, P21Control.Pool, P21ControlHyper.Pool, P56Control.Pool, P56ControlHyper.Pool)

Plot.1 <- ggplot(data = Mice.Pool, aes(x = Ti, y = f))+ 
  geom_point(size = 0.5, aes(colour = factor(V8)), show.legend = FALSE)+ 
  facet_grid(.~ V8)+
  scale_colour_manual(values = c("Red", "Green", "Chartreuse", "Blue", "Dodgerblue"))

Plot.2 <- ggplot(data = Mice.Pool, aes(x = Ti, y = TV))+ 
  geom_point(size = 0.5, aes(colour = factor(V8)), show.legend = FALSE)+ 
  facet_grid(.~ V8)+
  scale_colour_manual(values = c("Red", "Green", "Chartreuse", "Blue", "Dodgerblue"))

Plot.3 <- ggplot(Mice.Pool, aes(x=V8, y=f))+ 
  geom_violin(trim=FALSE, aes(fill = factor(V8)), show.legend = FALSE)+
  scale_fill_manual(values = c("Red", "Green", "Chartreuse", "Blue", "Dodgerblue"))+
  geom_boxplot(width=0.02)+
  xlab("")


Plot.4 <- ggplot(Mice.Pool, aes(x=V8, y=TV))+ 
  geom_violin(trim=FALSE, aes(fill = factor(V8)), show.legend = FALSE)+
  scale_fill_manual(values = c("Red", "Green", "Chartreuse", "Blue", "Dodgerblue"))+
  geom_boxplot(width=0.02)+
  xlab("")

Plot.5 <- ggplot(Mice.Pool, aes(x=V8, y=Ti))+ 
  geom_violin(trim=FALSE, aes(fill = factor(V8)), show.legend = FALSE)+
  scale_fill_manual(values = c("Red", "Green", "Chartreuse", "Blue", "Dodgerblue"))+
  geom_boxplot(width=0.02)+
  xlab("")


ggarrange(ggarrange(Plot.3, Plot.4, Plot.5, labels = c("A", "B", "C"), nrow = 1), Plot.1, Plot.2, labels= c("", "D", "E"), nrow = 3)