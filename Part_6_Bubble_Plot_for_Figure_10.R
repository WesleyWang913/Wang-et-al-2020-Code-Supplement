#Identify the relative effect mean and SD have on convex hull area and ellipse area
library(ggplot2)
library(dplyr)

#Polygon Area
my_fun <- function(x, y) {
  library(sp)
  library(rgeos)
  my_x <- c()
  my_y <- c()
  my_area <- c()
  for (i in x) {
    for (j in y) {
      my_number <- abs(rnorm(n = 100, mean = i, sd = j))
      data <- c(my_number[1:99], my_number[2:100])
      data <- matrix(data, nrow = 99)
      my_chull <- chull(data)
      my_chull <- c(my_chull, my_chull[1])
      my_polygon <- data[my_chull,]
      the_polygon <- Polygon(my_polygon, hole = FALSE)
      my_x <- append(my_x, mean(my_number))
      my_y <- append(my_y, sd(my_number))
      my_area <- append(my_area, the_polygon@area)
    }
  }
  my_df <- data.frame("mean" = my_x, "sd" = my_y, "area" = my_area)
  return(my_df)
}

my_data <- my_fun(x = 1:100, y = 1:100)
head(my_data)
summary(lm(area ~., data = my_data))

#Polygon area bubble plot
#Subset data to be able to visualize bubbles
sub_data <- my_data[sample(nrow(my_data), 500),]

plot1 <- ggplot(sub_data, aes(x=mean, y=sd, size=area))+
  geom_point(alpha=0.7, color= "blue")+
  scale_size(range= c(.1,7))+
  theme(legend.position = "none")+
  ggtitle("Polygon Area")


#lm(formula = area ~ ., data = my_data)
#Residuals:
#  Min     1Q Median     3Q    Max 
#-14562  -5039  -1688   3541  79816 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -17222.236    206.911  -83.23   <2e-16 ***
#  mean            82.358      3.895   21.14   <2e-16 ***
#  sd            1022.733      4.814  212.47   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7231 on 9997 degrees of freedom
#Multiple R-squared:  0.9008,	Adjusted R-squared:  0.9008 
#F-statistic: 4.538e+04 on 2 and 9997 DF,  p-value: < 2.2e-16

#Ellipse Area
my_ellipsefun <- function(x, y) {
  library(car)
  my_x <- c()
  my_y <- c()
  my_area <- c()
  for (i in x) {
    for (j in y) {
      my_number <- abs(rnorm(n = 100, mean = i, sd = j))
      data <- c(my_number[1:99], my_number[2:100])
      data <- matrix(data, nrow = 99)
      ell.info <- cov.wt(data)
      eigen.info <- eigen(ell.info$cov)
      lengths <- sqrt(eigen.info$values*2*qf(0.68,2,length(x)-1))
      area <- pi*lengths[1]*lengths[2]     
      my_x <- append(my_x, mean(my_number))
      my_y <- append(my_y, sd(my_number))
      my_area <- append(my_area, area)
    }
  }
  my_df <- data.frame("mean" = my_x, "sd" = my_y, "area" = my_area)
  return(my_df)
}

my_data <- my_ellipsefun(x = 1:100, y = 1:100)
head(my_data)
summary(lm(area ~., data = my_data))


#Ellipse Area Bubble Plot
sub_data <- my_data[sample(nrow(my_data), 400),]

plot2 <- ggplot(sub_data, aes(x=mean, y=sd, size=area))+
  geom_point(alpha=0.7, color="Red")+
  scale_size(range= c(.1,7))+
  theme(legend.position = "none")+
  ggtitle("Ellipse Area")

#Call:
#  lm(formula = area ~ ., data = my_data)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4965.8 -2159.5  -932.9  1569.1 16543.9 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -8686.834     83.397 -104.16   <2e-16 ***
#  mean           36.543      1.571   23.26   <2e-16 ***
#  sd            514.805      1.933  266.31   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2911 on 9997 degrees of freedom
#Multiple R-squared:  0.9335,	Adjusted R-squared:  0.9335 
#F-statistic: 7.014e+04 on 2 and 9997 DF,  p-value: < 2.2e-16

#Influence plot
poincare.frame <- data_frame()
poincare.frame <- data.frame(c("Polygon Area",
                               "Ellipse Area",
                               "Polygon Area",
                               "Ellipse Area"),
                             c(82.358/(82.358+1022.733), 
                               36.543/(36.543+ 514.805), 
                               1022.733/(82.358+1022.733),
                               514.805/(36.543+ 514.805)), 
                             c("Mean", 
                               "Mean",
                               "SD",
                               "SD"))
colnames(poincare.frame) <- c("Formula", "Influence", "Parameter")


plot3 <- ggplot(poincare.frame, aes(x=Parameter, y=Influence, fill=Formula)) +
  geom_bar(stat="identity", position=position_dodge(), color="black")+
  theme_minimal()+
  ylab("Relative Influece to Function")+
  scale_fill_manual(values = c("Red","Blue"))


ggarrange(plot2, plot1, plot3, nrow=1, ncol=3, labels=c("A","B","C"))
