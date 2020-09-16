#Function to make linear models for data sets
#-----
tablemkrf <- function(x,y) {
  xmod <- lm(f~NPIFb+NPEFb+Ti+Te+Tr, data=x)
  my_data <- data.frame("Intercept"=xmod$coefficients[1],"PIF"=xmod$coefficients[2], 
                        "PEF"=xmod$coefficients[3], "Ti"=xmod$coefficients[4], 
                        "Te"=xmod$coefficients[5], "Tr"=xmod$coefficients[6], "R-squared"=summary(xmod)$adj.r.squared, 
                        "P-value"= glance(xmod)$p.value, "Age"=y)
}
tablemkrTV <- function(x,y) {
  xmod <- lm(NTVb~NPIFb+NPEFb+Ti+Te+Tr, data=x)
  my_data <- data.frame("Intercept"=xmod$coefficients[1],"PIF"=xmod$coefficients[2], 
                        "PEF"=xmod$coefficients[3], "Ti"=xmod$coefficients[4], 
                        "Te"=xmod$coefficients[5], "Tr"=xmod$coefficients[6], "R-squared"=summary(xmod)$adj.r.squared, 
                        "P-value"= glance(xmod)$p.value, "Age"=y)
}

areas.f <- function(z){
  fsize <- length(z$f)
  fdata <- c(z$f[1:fsize-1],z$f[2:fsize])
  fdata <- matrix(fdata, nrow=fsize-1)
  fchull <- chull(fdata)
  fchull <- c(fchull, fchull[1])
  fpolygon <- fdata[fchull,]
  farea <- Polygon(fpolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(z$f[1:fsize-1],z$f[2:fsize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(z$f[1:fsize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.f <- data.frame("f Polygon Area" = farea, "f Ellipse Area" = ellarea)
}

areas.TV <- function(y){
  NTVbsize <- length(y$NTVb)
  NTVbdata <- c(y$NTVb[1:NTVbsize-1],y$NTVb[2:NTVbsize])
  NTVbdata <- matrix(NTVbdata, nrow=NTVbsize-1)
  NTVbchull <- chull(NTVbdata)
  NTVbchull <- c(NTVbchull, NTVbchull[1])
  NTVbpolygon <- NTVbdata[NTVbchull,]
  NTVbarea <- Polygon(NTVbpolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(y$NTVb[1:NTVbsize-1],y$NTVb[2:NTVbsize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(y$NTVb[1:NTVbsize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.TV <- data.frame("TV Polygon Area" = NTVbarea, "TV Ellipse Area" = ellarea)
}

areas.PIF <- function(y){
  NPIFbsize <- length(y$NPIFb)
  NPIFbdata <- c(y$NPIFb[1:NPIFbsize-1],y$NPIFb[2:NPIFbsize])
  NPIFbdata <- matrix(NPIFbdata, nrow=NPIFbsize-1)
  NPIFbchull <- chull(NPIFbdata)
  NPIFbchull <- c(NPIFbchull, NPIFbchull[1])
  NPIFbpolygon <- NPIFbdata[NPIFbchull,]
  NPIFbarea <- Polygon(NPIFbpolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(y$NPIFb[1:NPIFbsize-1],y$NPIFb[2:NPIFbsize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(y$NPIFb[1:NPIFbsize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.PIF <- data.frame("PIF Polygon Area" = NPIFbarea, "PIF Ellipse Area" = ellarea)
}

areas.PEF <- function(y){
  NPEFbsize <- length(y$NPEFb)
  NPEFbdata <- c(y$NPEFb[1:NPEFbsize-1],y$NPEFb[2:NPEFbsize])
  NPEFbdata <- matrix(NPEFbdata, nrow=NPEFbsize-1)
  NPEFbchull <- chull(NPEFbdata)
  NPEFbchull <- c(NPEFbchull, NPEFbchull[1])
  NPEFbpolygon <- NPEFbdata[NPEFbchull,]
  NPEFbarea <- Polygon(NPEFbpolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(y$NPEFb[1:NPEFbsize-1],y$NPEFb[2:NPEFbsize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(y$NPEFb[1:NPEFbsize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.PEF <- data.frame("PEF Polygon Area" = NPEFbarea, "PEF Ellipse Area" = ellarea)
}

areas.Ti <- function(y){
  Tisize <- length(y$Ti)
  Tidata <- c(y$Ti[1:Tisize-1],y$Ti[2:Tisize])
  Tidata <- matrix(Tidata, nrow=Tisize-1)
  Tichull <- chull(Tidata)
  Tichull <- c(Tichull, Tichull[1])
  Tipolygon <- Tidata[Tichull,]
  Tiarea <- Polygon(Tipolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(y$Ti[1:Tisize-1],y$Ti[2:Tisize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(y$Ti[1:Tisize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.Ti <- data.frame("Ti Polygon Area" = Tiarea, "Ti Ellipse Area" = ellarea)
}

areas.Te <- function(y){
  Tesize <- length(y$Te)
  Tedata <- c(y$Te[1:Tesize-1],y$Te[2:Tesize])
  Tedata <- matrix(Tedata, nrow=Tesize-1)
  Techull <- chull(Tedata)
  Techull <- c(Techull, Techull[1])
  Tepolygon <- Tedata[Techull,]
  Tearea <- Polygon(Tepolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(y$Te[1:Tesize-1],y$Te[2:Tesize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(y$Te[1:Tesize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.Te <- data.frame("Te Polygon Area" = Tearea, "Te Ellipse Area" = ellarea)
}

areas.Tr <- function(y){
  Trsize <- length(y$Tr)
  Trdata <- c(y$Tr[1:Trsize-1],y$Tr[2:Trsize])
  Trdata <- matrix(Trdata, nrow=Trsize-1)
  Trchull <- chull(Trdata)
  Trchull <- c(Trchull, Trchull[1])
  Trpolygon <- Trdata[Trchull,]
  Trarea <- Polygon(Trpolygon, hole = FALSE)@area
  ellinfo <- cov.wt(cbind(y$Tr[1:Trsize-1],y$Tr[2:Trsize]))
  eigeninfo <- eigen(ellinfo$cov)
  lengths <- sqrt(eigeninfo$values*2*qf(0.68,2,length(y$Tr[1:Trsize-1])-1))
  ellarea <- pi*lengths[1]*lengths[2]
  areas.Tr <- data.frame("Tr Polygon Area" = Trarea, "Tr Ellipse Area" = ellarea)
}

#Generate data frames for each group
#-----
#P1 Control Baseline
P1Control.fframe <- data.frame()
P1Control.areaframe <- data.frame()
P1Control.meanframe <- data.frame()
for (i in 1:9){
  model <- tablemkrf(get(paste("P1DataControl",i,sep="")),"P1")
  areas.1 <- areas.f(get(paste("P1DataControl",i,sep="")))
  areas.2 <- areas.TV(get(paste("P1DataControl",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P1DataControl",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P1DataControl",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P1DataControl",i,sep="")))
  areas.6 <- areas.Te(get(paste("P1DataControl",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P1DataControl",i,sep="")))
  areaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  P1Control.areaframe <- rbind(P1Control.areaframe,areaframe)
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P1Control.fframe <- rbind(P1Control.fframe,df)
}

P1Control.TVframe <- data.frame()
for (i in 1:9){
  model <- tablemkrTV(get(paste("P1DataControl",i,sep="")),"P1")
  areas.1 <- areas.f(get(paste("P1DataControl",i,sep="")))
  areas.2 <- areas.TV(get(paste("P1DataControl",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P1DataControl",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P1DataControl",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P1DataControl",i,sep="")))
  areas.6 <- areas.Te(get(paste("P1DataControl",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P1DataControl",i,sep="")))
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P1DataControl",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P1DataControl",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P1Control.TVframe <- rbind(P1Control.TVframe,df)
}


#P21 Control Baseline
P21Control.fframe <- data.frame()
P21Control.areaframe <- data.frame()
P21Control.meanframe <- data.frame()
for (i in 1:8){
  model <- tablemkrf(get(paste("P21DataControl",i,sep="")),"P21")
  areas.1 <- areas.f(get(paste("P21DataControl",i,sep="")))
  areas.2 <- areas.TV(get(paste("P21DataControl",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P21DataControl",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P21DataControl",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P21DataControl",i,sep="")))
  areas.6 <- areas.Te(get(paste("P21DataControl",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P21DataControl",i,sep="")))
  areaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  P21Control.areaframe <- rbind(P21Control.areaframe,areaframe)
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P21Control.fframe <- rbind(P21Control.fframe,df)
}

P21Control.TVframe <- data.frame()
for (i in 1:8){
  model <- tablemkrTV(get(paste("P21DataControl",i,sep="")),"P21")
  areas.1 <- areas.f(get(paste("P21DataControl",i,sep="")))
  areas.2 <- areas.TV(get(paste("P21DataControl",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P21DataControl",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P21DataControl",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P21DataControl",i,sep="")))
  areas.6 <- areas.Te(get(paste("P21DataControl",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P21DataControl",i,sep="")))
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P21DataControl",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P21DataControl",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P21Control.TVframe <- rbind(P21Control.TVframe,df)
}

#P21 Control Hypercapnic
P21ControlHyper.fframe <- data.frame()
P21ControlHyper.areaframe <- data.frame()
P21ControlHyper.meanframe <- data.frame()
for (i in 1:8){
  model <- tablemkrf(get(paste("P21DataControlHyper",i,sep="")),"P21")
  areas.1 <- areas.f(get(paste("P21DataControlHyper",i,sep="")))
  areas.2 <- areas.TV(get(paste("P21DataControlHyper",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P21DataControlHyper",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P21DataControlHyper",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P21DataControlHyper",i,sep="")))
  areas.6 <- areas.Te(get(paste("P21DataControlHyper",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P21DataControlHyper",i,sep="")))
  areaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  P21ControlHyper.areaframe <- rbind(P21ControlHyper.areaframe,areaframe)
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P21ControlHyper.fframe <- rbind(P21ControlHyper.fframe,df)
}

P21ControlHyper.TVframe <- data.frame()
for (i in 1:8){
  model <- tablemkrTV(get(paste("P21DataControlHyper",i,sep="")),"P21")
  areas.1 <- areas.f(get(paste("P21DataControlHyper",i,sep="")))
  areas.2 <- areas.TV(get(paste("P21DataControlHyper",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P21DataControlHyper",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P21DataControlHyper",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P21DataControlHyper",i,sep="")))
  areas.6 <- areas.Te(get(paste("P21DataControlHyper",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P21DataControlHyper",i,sep="")))
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P21DataControlHyper",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P21DataControlHyper",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P21ControlHyper.TVframe <- rbind(P21ControlHyper.TVframe,df)
}

#P56 Control Baseline
P56Control.fframe <- data.frame()
P56Control.areaframe <-data.frame()
P56Control.meanframe <- data.frame()
for (i in 1:9){
  model <- tablemkrf(get(paste("P56DataControl",i,sep="")),"P56")
  areas.1 <- areas.f(get(paste("P56DataControl",i,sep="")))
  areas.2 <- areas.TV(get(paste("P56DataControl",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P56DataControl",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P56DataControl",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P56DataControl",i,sep="")))
  areas.6 <- areas.Te(get(paste("P56DataControl",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P56DataControl",i,sep="")))
  areaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  P56Control.areaframe <- rbind(P56Control.areaframe,areaframe)
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P56Control.fframe <- rbind(P56Control.fframe,df)
}

P56Control.TVframe <- data.frame()
for (i in 1:9){
  model <- tablemkrTV(get(paste("P56DataControl",i,sep="")),"P56")
  areas.1 <- areas.f(get(paste("P56DataControl",i,sep="")))
  areas.2 <- areas.TV(get(paste("P56DataControl",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P56DataControl",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P56DataControl",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P56DataControl",i,sep="")))
  areas.6 <- areas.Te(get(paste("P56DataControl",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P56DataControl",i,sep="")))
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P56DataControl",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P56DataControl",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P56Control.TVframe <- rbind(P56Control.TVframe,df)
}

#P56 Control Hypercapnic
P56ControlHyper.fframe <- data.frame()
P56ControlHyper.areaframe <- data.frame()
P56ControlHyper.meanframe <- data.frame()
for (i in 1:9){
  model <- tablemkrf(get(paste("P56DataControlHyper",i,sep="")),"P56")
  areas.1 <- areas.f(get(paste("P56DataControlHyper",i,sep="")))
  areas.2 <- areas.TV(get(paste("P56DataControlHyper",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P56DataControlHyper",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P56DataControlHyper",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P56DataControlHyper",i,sep="")))
  areas.6 <- areas.Te(get(paste("P56DataControlHyper",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P56DataControlHyper",i,sep="")))
  areaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  P56ControlHyper.areaframe <- rbind(P56ControlHyper.areaframe,areaframe)
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P56ControlHyper.fframe <- rbind(P56ControlHyper.fframe,df)
}

P56ControlHyper.TVframe <- data.frame()
for (i in 1:9){
  model <- tablemkrTV(get(paste("P56DataControlHyper",i,sep="")),"P56")
  areas.1 <- areas.f(get(paste("P56DataControlHyper",i,sep="")))
  areas.2 <- areas.TV(get(paste("P56DataControlHyper",i,sep="")))
  areas.3 <- areas.PIF(get(paste("P56DataControlHyper",i,sep="")))
  areas.4 <- areas.PEF(get(paste("P56DataControlHyper",i,sep="")))
  areas.5 <- areas.Ti(get(paste("P56DataControlHyper",i,sep="")))
  areas.6 <- areas.Te(get(paste("P56DataControlHyper",i,sep="")))
  areas.7 <- areas.Tr(get(paste("P56DataControlHyper",i,sep="")))
  areas.1[,1] <- areas.1[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,1]))
  areas.1[,2] <- areas.1[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,1]))
  areas.2[,1] <- areas.2[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,2]))
  areas.2[,2] <- areas.2[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,2]))
  areas.3[,1] <- areas.3[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,3]))
  areas.3[,2] <- areas.3[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,3]))
  areas.4[,1] <- areas.4[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,4]))
  areas.4[,2] <- areas.4[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,4]))
  areas.5[,1] <- areas.5[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,5]))
  areas.5[,2] <- areas.5[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,5]))
  areas.6[,1] <- areas.6[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,6]))
  areas.6[,2] <- areas.6[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,6]))
  areas.7[,1] <- areas.7[,1]-(82.358*mean(get(paste("P56DataControlHyper",i,sep=""))[,7]))
  areas.7[,2] <- areas.7[,2]-(36.543*mean(get(paste("P56DataControlHyper",i,sep=""))[,7]))
  correctedareaframe <- data.frame(areas.1, areas.2, areas.3, areas.4, areas.5, areas.6, areas.7)
  df <- data.frame(model,correctedareaframe)
  P56ControlHyper.TVframe <- rbind(P56ControlHyper.TVframe,df)
}

#f means
P21.consolidationframe <- P21Control.areaframe[,1:14] - P21ControlHyper.areaframe[,1:14]
P56.consolidationframe <- P56Control.areaframe[,1:14] - P56ControlHyper.areaframe[,1:14]
t.test(P21.consolidationframe[,1], P56.consolidationframe[,1]) #0.4041
t.test(P21.consolidationframe[,2], P56.consolidationframe[,2]) #0.598
t.test(P21.consolidationframe[,3], P56.consolidationframe[,3]) #0.8611
t.test(P21.consolidationframe[,4], P56.consolidationframe[,4]) #0.9792
t.test(P21.consolidationframe[,5], P56.consolidationframe[,5]) #0.03389*
t.test(P21.consolidationframe[,6], P56.consolidationframe[,6]) #0.1044
t.test(P21.consolidationframe[,7], P56.consolidationframe[,7]) #0.7265
t.test(P21.consolidationframe[,8], P56.consolidationframe[,8]) #0.6431
t.test(P21.consolidationframe[,9], P56.consolidationframe[,9]) #0.9032
t.test(P21.consolidationframe[,10], P56.consolidationframe[,10]) 
t.test(P21.consolidationframe[,11], P56.consolidationframe[,11])
t.test(P21.consolidationframe[,12], P56.consolidationframe[,12])
t.test(P21.consolidationframe[,13], P56.consolidationframe[,13])
t.test(P21.consolidationframe[,14], P56.consolidationframe[,14])


t.test(P1Control.meanframe[,1], P21Control.meanframe[,1]) #p 2.264e-07 *
t.test(P1Control.meanframe[,1], P56Control.meanframe[,1]) #p 0.001827 *
t.test(P1Control.meanframe[,1], P21ControlHyper.meanframe[,1]) #p 7.211e-07*
t.test(P1Control.meanframe[,1], P56ControlHyper.meanframe[,1]) #p 0.0006999*
t.test(P21Control.meanframe[,1], P21ControlHyper.meanframe[,1]) #p 0.0.04332*
t.test(P21Control.meanframe[,1], P56Control.meanframe[,1]) #p 0.2933
t.test(P21Control.meanframe[,1], P56ControlHyper.meanframe[,1]) #p 0.5394
t.test(P56Control.meanframe[,1], P56ControlHyper.meanframe[,1]) #p 0.7077
t.test(P21ControlHyper.meanframe[,1], P56ControlHyper.meanframe[,1]) #p 0.08579 



#f polygon area
t.test(P1Control.areaframe[,1], P21Control.areaframe[,1]) #p 0.0004433 *
t.test(P1Control.areaframe[,1], P56Control.areaframe[,1]) #p 0.0003607 *
t.test(P1Control.areaframe[,1], P21ControlHyper.areaframe[,1]) #p 0.04828*
t.test(P1Control.areaframe[,1], P56ControlHyper.areaframe[,1]) #p 0.0213* 
t.test(P21Control.areaframe[,1], P21ControlHyper.areaframe[,1]) #p 0.0005973 *
t.test(P21Control.areaframe[,1], P56Control.areaframe[,1]) #p 0.904
t.test(P21Control.areaframe[,1], P56ControlHyper.areaframe[,1]) #p 0.002535 *
t.test(P56Control.areaframe[,1], P56ControlHyper.areaframe[,1]) #p 0.002552 *
t.test(P21ControlHyper.areaframe[,1], P56ControlHyper.areaframe[,1]) #p 0.06254 

#f ellipse area
t.test(P1Control.areaframe[,2], P21Control.areaframe[,2]) #p 0.0006931 *
t.test(P1Control.areaframe[,2], P56Control.areaframe[,2]) #p 0.0005238 *
t.test(P1Control.areaframe[,2], P21ControlHyper.areaframe[,2]) #p 0.06495
t.test(P1Control.areaframe[,2], P56ControlHyper.areaframe[,2]) #p 0.02819 *
t.test(P21Control.areaframe[,2], P21ControlHyper.areaframe[,2]) #p 0.0009807 *
t.test(P21Control.areaframe[,2], P56Control.areaframe[,2]) #p 0.6705
t.test(P21Control.areaframe[,2], P56ControlHyper.areaframe[,2]) #p 0.006665 *
t.test(P56Control.areaframe[,2], P56ControlHyper.areaframe[,2]) #p 0.004494 *
t.test(P21ControlHyper.areaframe[,2], P56ControlHyper.areaframe[,2]) #p 0.08196 

#tv means
t.test(P1Control.meanframe[,2], P21Control.meanframe[,2]) #p 4.25e-08 *
t.test(P1Control.meanframe[,2], P56Control.meanframe[,2]) #p 9.832e-05 *
t.test(P1Control.meanframe[,2], P21ControlHyper.meanframe[,2]) #p 1.016e-06 *
t.test(P1Control.meanframe[,2], P56ControlHyper.meanframe[,2]) #p 1.308e-07 *
t.test(P21Control.meanframe[,2], P21ControlHyper.meanframe[,2]) #p 0.0001726 *
t.test(P21Control.meanframe[,2], P56Control.meanframe[,2]) #p 0.2345
t.test(P21Control.meanframe[,2], P56ControlHyper.meanframe[,2]) #p 0.005167 *
t.test(P56Control.meanframe[,2], P56ControlHyper.meanframe[,2]) #p 0.002131 *
t.test(P21ControlHyper.meanframe[,2], P56ControlHyper.meanframe[,2]) #p 0.01488

#tv polygon area
t.test(P1Control.areaframe[,3], P21Control.areaframe[,3]) #p 2.042e-05 *
t.test(P1Control.areaframe[,3], P56Control.areaframe[,3]) #p 0.00342 *
t.test(P1Control.areaframe[,3], P21ControlHyper.areaframe[,3]) #p 0.004532 *
t.test(P1Control.areaframe[,3], P56ControlHyper.areaframe[,3]) #p 0.001027 * 
t.test(P21Control.areaframe[,3], P21ControlHyper.areaframe[,3]) #p 0.001884 *
t.test(P21Control.areaframe[,3], P56Control.areaframe[,3]) #p 0.4031
t.test(P21Control.areaframe[,3], P56ControlHyper.areaframe[,3]) #p 8.832e-05 *
t.test(P56Control.areaframe[,3], P56ControlHyper.areaframe[,3]) #p 0.02747*
t.test(P21ControlHyper.areaframe[,3], P56ControlHyper.areaframe[,3]) #p 0.2417

#tv ellipse area
t.test(P1Control.areaframe[,4], P21Control.areaframe[,4]) #p 2.193e-05 *
t.test(P1Control.areaframe[,4], P56Control.areaframe[,4]) #p 0.00471 *
t.test(P1Control.areaframe[,4], P21ControlHyper.areaframe[,4]) #p 0.007787 *
t.test(P1Control.areaframe[,4], P56ControlHyper.areaframe[,4]) #p 0.004492 * 
t.test(P21Control.areaframe[,4], P21ControlHyper.areaframe[,4]) #p 0.005705 *
t.test(P21Control.areaframe[,4], P56Control.areaframe[,4]) #p 0.539
t.test(P21Control.areaframe[,4], P56ControlHyper.areaframe[,4]) #p 0.0002236 *
t.test(P56Control.areaframe[,4], P56ControlHyper.areaframe[,4]) #p 0.0365*
t.test(P21ControlHyper.areaframe[,4], P56ControlHyper.areaframe[,4]) #p 0.2417

#PIF means
t.test(P1Control.meanframe[,3], P21Control.meanframe[,3]) #p 4.064e-06 *
t.test(P1Control.meanframe[,3], P56Control.meanframe[,3]) #p 4e-06 *
t.test(P1Control.meanframe[,3], P21ControlHyper.meanframe[,3]) #p 3.942e-05 *
t.test(P1Control.meanframe[,3], P56ControlHyper.meanframe[,3]) #p 1.223e-05 *
t.test(P21Control.meanframe[,3], P21ControlHyper.meanframe[,3]) #p 0.2338
t.test(P21Control.meanframe[,3], P56Control.meanframe[,3]) #p 0.03508*
t.test(P21Control.meanframe[,3], P56ControlHyper.meanframe[,3]) #p 0.08198
t.test(P56Control.meanframe[,3], P56ControlHyper.meanframe[,3]) #p 0.7089
t.test(P21ControlHyper.meanframe[,3], P56ControlHyper.meanframe[,3]) #p 0.02062*

#PIF polygon area
t.test(P1Control.areaframe[,6], P21Control.areaframe[,6]) #p 6.392e-05 *
t.test(P1Control.areaframe[,5], P56Control.areaframe[,5]) #p 0.0002086 *
t.test(P1Control.areaframe[,5], P21ControlHyper.areaframe[,5]) #p 0.007871 *
t.test(P1Control.areaframe[,5], P56ControlHyper.areaframe[,5]) #p 0.00251 * 
t.test(P21Control.areaframe[,5], P21ControlHyper.areaframe[,5]) #p 0.00299 *
t.test(P21Control.areaframe[,5], P56Control.areaframe[,5]) #p 0.01196*
t.test(P21Control.areaframe[,5], P56ControlHyper.areaframe[,5]) #p 0.0001958 *
t.test(P56Control.areaframe[,5], P56ControlHyper.areaframe[,5]) #p 0.01142 *
t.test(P21ControlHyper.areaframe[,5], P56ControlHyper.areaframe[,5]) #p 0.2462

#PIF ellipse area
t.test(P1Control.areaframe[,6], P21Control.areaframe[,6]) #p 0.0001535 *
t.test(P1Control.areaframe[,6], P56Control.areaframe[,6]) #p 0.0003624 *
t.test(P1Control.areaframe[,6], P21ControlHyper.areaframe[,6]) #p 0.01493 *
t.test(P1Control.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.003851 * 
t.test(P21Control.areaframe[,6], P21ControlHyper.areaframe[,6]) #p 0.02641 *
t.test(P21Control.areaframe[,6], P56Control.areaframe[,6]) #p 0.03076*
t.test(P21Control.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.001131*
t.test(P56Control.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.05271 
t.test(P21ControlHyper.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.349

#PEF means
t.test(P1Control.meanframe[,4], P21Control.meanframe[,4]) #p 5.14e-07 *
t.test(P1Control.meanframe[,4], P56Control.meanframe[,4]) #p 0.0001378 *
t.test(P1Control.meanframe[,4], P21ControlHyper.meanframe[,4]) #p 2.858e-05 *
t.test(P1Control.meanframe[,4], P56ControlHyper.meanframe[,4]) #p 8.069e-06 *
t.test(P21Control.meanframe[,4], P21ControlHyper.meanframe[,4]) #p 0.0.000905 *
t.test(P21Control.meanframe[,4], P56Control.meanframe[,4]) #p 0.7908
t.test(P21Control.meanframe[,4], P56ControlHyper.meanframe[,4]) #p 0.002885 *
t.test(P56Control.meanframe[,4], P56ControlHyper.meanframe[,4]) #p 0.009058 *
t.test(P21ControlHyper.meanframe[,4], P56ControlHyper.meanframe[,4]) #p 0.1154

#PEF polygon area
t.test(P1Control.areaframe[,7], P21Control.areaframe[,7]) #p 0.001363 *
t.test(P1Control.areaframe[,7], P56Control.areaframe[,7]) #p 0.0158 *
t.test(P1Control.areaframe[,7], P21ControlHyper.areaframe[,7]) #p 0.004757 *
t.test(P1Control.areaframe[,7], P56ControlHyper.areaframe[,7]) #p 0.0007165 * 
t.test(P21Control.areaframe[,7], P21ControlHyper.areaframe[,7]) #p 0.0294 *
t.test(P21Control.areaframe[,7], P56Control.areaframe[,7]) #p 0.5888
t.test(P21Control.areaframe[,7], P56ControlHyper.areaframe[,7]) #p 0.06713
t.test(P56Control.areaframe[,7], P56ControlHyper.areaframe[,7]) #p 0.1302 
t.test(P21ControlHyper.areaframe[,7], P56ControlHyper.areaframe[,7]) #p 0.4806

#PEF ellipse area
t.test(P1Control.areaframe[,6], P21Control.areaframe[,6]) #p 0.0001535 *
t.test(P1Control.areaframe[,6], P56Control.areaframe[,6]) #p 0.0003624 *
t.test(P1Control.areaframe[,6], P21ControlHyper.areaframe[,6]) #p 0.01493 *
t.test(P1Control.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.003851 * 
t.test(P21Control.areaframe[,6], P21ControlHyper.areaframe[,6]) #p 0.02641 *
t.test(P21Control.areaframe[,6], P56Control.areaframe[,6]) #p 0.03076*
t.test(P21Control.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.001131*
t.test(P56Control.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.05271 
t.test(P21ControlHyper.areaframe[,6], P56ControlHyper.areaframe[,6]) #p 0.349

#Ti means
t.test(P1Control.meanframe[,5], P21Control.meanframe[,5]) #p 1.967e-05 *
t.test(P1Control.meanframe[,5], P56Control.meanframe[,5]) #p 2.6E-05 *
t.test(P1Control.meanframe[,5], P21ControlHyper.meanframe[,5]) #p 4.951e-05 *
t.test(P1Control.meanframe[,5], P56ControlHyper.meanframe[,5]) #p 7.372e-05 *
t.test(P21Control.meanframe[,5], P21ControlHyper.meanframe[,5]) #p 0.05971
t.test(P21Control.meanframe[,5], P56Control.meanframe[,5]) #p 0.2049
t.test(P21Control.meanframe[,5], P56ControlHyper.meanframe[,5]) #p 0.009575*
t.test(P56Control.meanframe[,5], P56ControlHyper.meanframe[,5]) #p 0.1739
t.test(P21ControlHyper.meanframe[,5], P56ControlHyper.meanframe[,5]) #p 0.1303

#Ti polygon area
t.test(P1Control.areaframe[,9], P21Control.areaframe[,9]) #p 0.05552 
t.test(P1Control.areaframe[,9], P56Control.areaframe[,9]) #p 0.07133
t.test(P1Control.areaframe[,9], P21ControlHyper.areaframe[,9]) #p 0.0312 *
t.test(P1Control.areaframe[,9], P56ControlHyper.areaframe[,9]) #p 0.04138 *
t.test(P21Control.areaframe[,9], P21ControlHyper.areaframe[,9]) #p 0.007539 *
t.test(P21Control.areaframe[,9], P56Control.areaframe[,9]) #p 0.2436
t.test(P21Control.areaframe[,9], P56ControlHyper.areaframe[,9]) #p 0.1598
t.test(P56Control.areaframe[,9], P56ControlHyper.areaframe[,9]) #p 0.008771 *
t.test(P21ControlHyper.areaframe[,9], P56ControlHyper.areaframe[,9]) #p 0.04926

#Ti ellipse area
t.test(P1Control.areaframe[,10], P21Control.areaframe[,10]) #p 0.04924 * 
t.test(P1Control.areaframe[,10], P56Control.areaframe[,10]) #p 0.06025
t.test(P1Control.areaframe[,10], P21ControlHyper.areaframe[,10]) #p 0.03358*
t.test(P1Control.areaframe[,10], P56ControlHyper.areaframe[,10]) #p 0.04348 *
t.test(P21Control.areaframe[,10], P21ControlHyper.areaframe[,10]) #p 0.003296 *
t.test(P21Control.areaframe[,10], P56Control.areaframe[,10]) #p 0.1412
t.test(P21Control.areaframe[,10], P56ControlHyper.areaframe[,10]) #p 0.4265
t.test(P56Control.areaframe[,10], P56ControlHyper.areaframe[,10]) #p 0.05296
t.test(P21ControlHyper.areaframe[,10], P56ControlHyper.areaframe[,10]) #p 0.0795

#Te means
t.test(P1Control.meanframe[,6], P21Control.meanframe[,6]) #p 2.159e-07 *
t.test(P1Control.meanframe[,6], P56Control.meanframe[,6]) #p 0.0004498 *
t.test(P1Control.meanframe[,6], P21ControlHyper.meanframe[,6]) #p 1.27e-07 *
t.test(P1Control.meanframe[,6], P56ControlHyper.meanframe[,6]) #p 1.105e-06 *
t.test(P21Control.meanframe[,6], P21ControlHyper.meanframe[,6]) #p 0.0.0001019 *
t.test(P21Control.meanframe[,6], P56Control.meanframe[,6]) #p 0.08231
t.test(P21Control.meanframe[,6], P56ControlHyper.meanframe[,6]) #p 0.724
t.test(P56Control.meanframe[,6], P56ControlHyper.meanframe[,6]) #p 0.18168262
t.test(P21ControlHyper.meanframe[,6], P56ControlHyper.meanframe[,6]) #p 0.03775

#Te polygon area
t.test(P1Control.areaframe[,11], P21Control.areaframe[,11]) #p 0.06077
t.test(P1Control.areaframe[,11], P56Control.areaframe[,11]) #p 0.9821
t.test(P1Control.areaframe[,11], P21ControlHyper.areaframe[,11]) #p 0.00747 *
t.test(P1Control.areaframe[,11], P56ControlHyper.areaframe[,11]) #p 0.02497 *
t.test(P21Control.areaframe[,11], P21ControlHyper.areaframe[,11]) #p 0.0102 *
t.test(P21Control.areaframe[,11], P56Control.areaframe[,11]) #p 0.1225
t.test(P21Control.areaframe[,11], P56ControlHyper.areaframe[,11]) #p 0.3191
t.test(P56Control.areaframe[,11], P56ControlHyper.areaframe[,11]) #p 0.06228
t.test(P21ControlHyper.areaframe[,11], P56ControlHyper.areaframe[,11]) #p 0.08664

#Te ellipse area
t.test(P1Control.areaframe[,12], P21Control.areaframe[,12]) #p 0.04852*
t.test(P1Control.areaframe[,12], P56Control.areaframe[,12]) #p 0.7446
t.test(P1Control.areaframe[,12], P21ControlHyper.areaframe[,12]) #p 0.01077 *
t.test(P1Control.areaframe[,12], P56ControlHyper.areaframe[,12]) #p 0.02908 *
t.test(P21Control.areaframe[,12], P21ControlHyper.areaframe[,12]) #p 0.01289 *
t.test(P21Control.areaframe[,12], P56Control.areaframe[,12]) #p 0.1408
t.test(P21Control.areaframe[,12], P56ControlHyper.areaframe[,12]) #p 0.4506
t.test(P56Control.areaframe[,12], P56ControlHyper.areaframe[,12]) #p 0.09096
t.test(P21ControlHyper.areaframe[,12], P56ControlHyper.areaframe[,12]) #p 0.08233

#Tr means
t.test(P1Control.meanframe[,7], P21Control.meanframe[,7]) #p 0.001153 *
t.test(P1Control.meanframe[,7], P56Control.meanframe[,7]) #p 0.1087
t.test(P1Control.meanframe[,7], P21ControlHyper.meanframe[,7]) #p 0.001765 *
t.test(P1Control.meanframe[,7], P56ControlHyper.meanframe[,7]) #p 0.01568 *
t.test(P21Control.meanframe[,7], P21ControlHyper.meanframe[,7]) #p 4.096e-05*
t.test(P21Control.meanframe[,7], P56Control.meanframe[,7]) #p 0.09013
t.test(P21Control.meanframe[,7], P56ControlHyper.meanframe[,7]) #p 0.449
t.test(P56Control.meanframe[,7], P56ControlHyper.meanframe[,7]) #p 0.3969
t.test(P21ControlHyper.meanframe[,7], P56ControlHyper.meanframe[,7]) #p 0.04992 *

#Tr polygon area
t.test(P1Control.areaframe[,13], P21Control.areaframe[,13]) #p 0.0879
t.test(P1Control.areaframe[,13], P56Control.areaframe[,13]) #p 0.6866
t.test(P1Control.areaframe[,13], P21ControlHyper.areaframe[,13]) #p 0.05104
t.test(P1Control.areaframe[,13], P56ControlHyper.areaframe[,13]) #p 0.1122
t.test(P21Control.areaframe[,13], P21ControlHyper.areaframe[,13]) #p 0.02972 *
t.test(P21Control.areaframe[,13], P56Control.areaframe[,13]) #p 0.09644
t.test(P21Control.areaframe[,13], P56ControlHyper.areaframe[,13]) #p 0.647
t.test(P56Control.areaframe[,13], P56ControlHyper.areaframe[,13]) #p 0.1353
t.test(P21ControlHyper.areaframe[,13], P56ControlHyper.areaframe[,13]) #p 0.1

#Tr ellipse area
t.test(P1Control.areaframe[,14], P21Control.areaframe[,14]) #p 0.09634
t.test(P1Control.areaframe[,14], P56Control.areaframe[,14]) #p 0.3348
t.test(P1Control.areaframe[,14], P21ControlHyper.areaframe[,14]) #p 0.07239
t.test(P1Control.areaframe[,14], P56ControlHyper.areaframe[,14]) #p 0.118
t.test(P21Control.areaframe[,14], P21ControlHyper.areaframe[,14]) #p 0.0.03806 *
t.test(P21Control.areaframe[,14], P56Control.areaframe[,14]) #p 0.1083
t.test(P21Control.areaframe[,14], P56ControlHyper.areaframe[,14]) #p 0.4992
t.test(P56Control.areaframe[,14], P56ControlHyper.areaframe[,14]) #p 0.1799
t.test(P21ControlHyper.areaframe[,14], P56ControlHyper.areaframe[,14]) #p 0.09363