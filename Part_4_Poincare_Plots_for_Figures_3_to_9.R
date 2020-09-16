#Poincare Plots
#-----
#Frequency
#P1 Control f Baseline
P1Control.fsize <- length(P1Control.Pool$f)
P1Control.fdata <- c(P1Control.Pool$f[1:P1Control.fsize-1]P1Control.Pool$f[2:P1Control.fsize])
P1Control.fdata <- matrix(P1Control.fdata, nrow=P1Control.fsize-1)
P1Control.fchull <- chull(P1Control.fdata)
P1Control.fchull <- c(P1Control.fchull, P1Control.fchull[1])
P1Control.fpolygon <- P1Control.fdata[P1Control.fchull,]
P1Control.farea <- Polygon(P1Control.fpolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$f[1:P1Control.fsize-1], P1Control.Pool$f[2:P1Control.fsize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$f[1:P1Control.fsize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control f Baseline
P21Control.fsize <- length(P21Control.Pool$f)
P21Control.fdata <- c(P21Control.Pool$f[1:P21Control.fsize-1],P21Control.Pool$f[2:P21Control.fsize])
P21Control.fdata <- matrix(P21Control.fdata, nrow=P21Control.fsize-1)
P21Control.fchull <- chull(P21Control.fdata)
P21Control.fchull <- c(P21Control.fchull, P21Control.fchull[1])
P21Control.fpolygon <- P21Control.fdata[P21Control.fchull,]
P21Control.farea <- Polygon(P21Control.fpolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$f[1:P21Control.fsize-1], P21Control.Pool$f[2:P21Control.fsize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$f[1:P21Control.fsize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control f Baseline
P56Control.fsize <- length(P56Control.Pool$f)
P56Control.fdata <- c(P56Control.Pool$f[1:P56Control.fsize-1],P56Control.Pool$f[2:P56Control.fsize])
P56Control.fdata <- matrix(P56Control.fdata, nrow=P56Control.fsize-1)
P56Control.fchull <- chull(P56Control.fdata)
P56Control.fchull <- c(P56Control.fchull, P56Control.fchull[1])
P56Control.fpolygon <- P56Control.fdata[P56Control.fchull,]
P56Control.farea <- Polygon(P56Control.fpolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$f[1:P56Control.fsize-1], P56Control.Pool$f[2:P56Control.fsize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$f[1:P56Control.fsize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 Control f Hypercapnia
P21ControlHyper.fsize <- length(P21ControlHyper.Pool$f)
P21ControlHyper.fdata <- c(P21ControlHyper.Pool$f[1:P21ControlHyper.fsize-1],P21ControlHyper.Pool$f[2:P21ControlHyper.fsize])
P21ControlHyper.fdata <- matrix(P21ControlHyper.fdata, nrow=P21ControlHyper.fsize-1)
P21ControlHyper.fchull <- chull(P21ControlHyper.fdata)
P21ControlHyper.fchull <- c(P21ControlHyper.fchull, P21ControlHyper.fchull[1])
P21ControlHyper.fpolygon <- P21ControlHyper.fdata[P21ControlHyper.fchull,]
P21ControlHyper.farea <- Polygon(P21ControlHyper.fpolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$f[1:P21ControlHyper.fsize-1], P21ControlHyper.Pool$f[2:P21ControlHyper.fsize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$f[1:P21ControlHyper.fsize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 Control f Hypercapnia
P56ControlHyper.fsize <- length(P56ControlHyper.Pool$f)
P56ControlHyper.fdata <- c(P56ControlHyper.Pool$f[1:P56ControlHyper.fsize-1],P56ControlHyper.Pool$f[2:P56ControlHyper.fsize])
P56ControlHyper.fdata <- matrix(P56ControlHyper.fdata, nrow=P56ControlHyper.fsize-1)
P56ControlHyper.fchull <- chull(P56ControlHyper.fdata)
P56ControlHyper.fchull <- c(P56ControlHyper.fchull, P56ControlHyper.fchull[1])
P56ControlHyper.fpolygon <- P56ControlHyper.fdata[P56ControlHyper.fchull,]
P56ControlHyper.farea <- Polygon(P56ControlHyper.fpolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$f[1:P56ControlHyper.fsize-1], P56ControlHyper.Pool$f[2:P56ControlHyper.fsize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$f[1:P56ControlHyper.fsize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

fhullarea.table <- c(P1Control.farea, P21Control.farea, P56Control.farea, P21ControlHyper.farea, P56ControlHyper.farea)
fellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea,P21ControlHyper.ellarea, P56ControlHyper.ellarea)
fcenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#Tidal Volume
#P1 Control TV Baseline
P1Control.TVsize <- length(P1Control.Pool$TV)
P1Control.TVdata <- c(P1Control.Pool$TV[1:P1Control.TVsize-1],P1Control.Pool$TV[2:P1Control.TVsize])
P1Control.TVdata <- matrix(P1Control.TVdata, nrow=P1Control.TVsize-1)
P1Control.TVchull <- chull(P1Control.TVdata)
P1Control.TVchull <- c(P1Control.TVchull, P1Control.TVchull[1])
P1Control.TVpolygon <- P1Control.TVdata[P1Control.TVchull,]
P1Control.TVarea <- Polygon(P1Control.TVpolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$TV[1:P1Control.TVsize-1], P1Control.Pool$TV[2:P1Control.TVsize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$f[1:P1Control.fsize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control TV Baseline
P21Control.TVsize <- length(P21Control.Pool$TV)
P21Control.TVdata <- c(P21Control.Pool$TV[1:P21Control.TVsize-1],P21Control.Pool$TV[2:P21Control.TVsize])
P21Control.TVdata <- matrix(P21Control.TVdata, nrow=P21Control.TVsize-1)
P21Control.TVchull <- chull(P21Control.TVdata)
P21Control.TVchull <- c(P21Control.TVchull, P21Control.TVchull[1])
P21Control.TVpolygon <- P21Control.TVdata[P21Control.TVchull,]
P21Control.TVarea <- Polygon(P21Control.TVpolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$TV[1:P21Control.TVsize-1], P21Control.Pool$TV[2:P21Control.TVsize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$f[1:P21Control.fsize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control TV Baseline
P56Control.TVsize <- length(P56Control.Pool$TV)
P56Control.TVdata <- c(P56Control.Pool$TV[1:P56Control.TVsize-1],P56Control.Pool$TV[2:P56Control.TVsize])
P56Control.TVdata <- matrix(P56Control.TVdata, nrow=P56Control.TVsize-1)
P56Control.TVchull <- chull(P56Control.TVdata)
P56Control.TVchull <- c(P56Control.TVchull, P56Control.TVchull[1])
P56Control.TVpolygon <- P56Control.TVdata[P56Control.TVchull,]
P56Control.TVarea <- Polygon(P56Control.TVpolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$TV[1:P56Control.TVsize-1], P56Control.Pool$TV[2:P56Control.TVsize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$f[1:P56Control.fsize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 ControlHyper TV Baseline
P21ControlHyper.TVsize <- length(P21ControlHyper.Pool$TV)
P21ControlHyper.TVdata <- c(P21ControlHyper.Pool$TV[1:P21ControlHyper.TVsize-1],P21ControlHyper.Pool$TV[2:P21ControlHyper.TVsize])
P21ControlHyper.TVdata <- matrix(P21ControlHyper.TVdata, nrow=P21ControlHyper.TVsize-1)
P21ControlHyper.TVchull <- chull(P21ControlHyper.TVdata)
P21ControlHyper.TVchull <- c(P21ControlHyper.TVchull, P21ControlHyper.TVchull[1])
P21ControlHyper.TVpolygon <- P21ControlHyper.TVdata[P21ControlHyper.TVchull,]
P21ControlHyper.TVarea <- Polygon(P21ControlHyper.TVpolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$TV[1:P21ControlHyper.TVsize-1], P21ControlHyper.Pool$TV[2:P21ControlHyper.TVsize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$f[1:P21ControlHyper.fsize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 ControlHyper TV Baseline
P56ControlHyper.TVsize <- length(P56ControlHyper.Pool$TV)
P56ControlHyper.TVdata <- c(P56ControlHyper.Pool$TV[1:P56ControlHyper.TVsize-1],P56ControlHyper.Pool$TV[2:P56ControlHyper.TVsize])
P56ControlHyper.TVdata <- matrix(P56ControlHyper.TVdata, nrow=P56ControlHyper.TVsize-1)
P56ControlHyper.TVchull <- chull(P56ControlHyper.TVdata)
P56ControlHyper.TVchull <- c(P56ControlHyper.TVchull, P56ControlHyper.TVchull[1])
P56ControlHyper.TVpolygon <- P56ControlHyper.TVdata[P56ControlHyper.TVchull,]
P56ControlHyper.TVarea <- Polygon(P56ControlHyper.TVpolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$TV[1:P56ControlHyper.TVsize-1], P56ControlHyper.Pool$TV[2:P56ControlHyper.TVsize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$f[1:P56ControlHyper.fsize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

TVarea.table <- c(P1Control.TVarea, P21Control.TVarea, P56Control.TVarea, P21ControlHyper.TVarea, P56ControlHyper.TVarea)
TVellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea,P21ControlHyper.ellarea, P56ControlHyper.ellarea)
TVcenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#PIF
#P1 Control PIF Baseline
P1Control.PIFsize <- length(P1Control.Pool$PIF)
P1Control.PIFdata <- c(P1Control.Pool$PIF[1:P1Control.PIFsize-1],P1Control.Pool$PIF[2:P1Control.PIFsize])
P1Control.PIFdata <- matrix(P1Control.PIFdata, nrow=P1Control.PIFsize-1)
P1Control.PIFchull <- chull(P1Control.PIFdata)
P1Control.PIFchull <- c(P1Control.PIFchull, P1Control.PIFchull[1])
P1Control.PIFpolygon <- P1Control.PIFdata[P1Control.PIFchull,]
P1Control.PIFarea <- Polygon(P1Control.PIFpolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$PIF[1:P1Control.PIFsize-1], P1Control.Pool$PIF[2:P1Control.PIFsize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$PIF[1:P1Control.PIFsize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control PIF Baseline
P21Control.PIFsize <- length(P21Control.Pool$PIF)
P21Control.PIFdata <- c(P21Control.Pool$PIF[1:P21Control.PIFsize-1],P21Control.Pool$PIF[2:P21Control.PIFsize])
P21Control.PIFdata <- matrix(P21Control.PIFdata, nrow=P21Control.PIFsize-1)
P21Control.PIFchull <- chull(P21Control.PIFdata)
P21Control.PIFchull <- c(P21Control.PIFchull, P21Control.PIFchull[1])
P21Control.PIFpolygon <- P21Control.PIFdata[P21Control.PIFchull,]
P21Control.PIFarea <- Polygon(P21Control.PIFpolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$PIF[1:P21Control.PIFsize-1], P21Control.Pool$PIF[2:P21Control.PIFsize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$PIF[1:P21Control.PIFsize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control PIF Baseline
P56Control.PIFsize <- length(P56Control.Pool$PIF)
P56Control.PIFdata <- c(P56Control.Pool$PIF[1:P56Control.PIFsize-1],P56Control.Pool$PIF[2:P56Control.PIFsize])
P56Control.PIFdata <- matrix(P56Control.PIFdata, nrow=P56Control.PIFsize-1)
P56Control.PIFchull <- chull(P56Control.PIFdata)
P56Control.PIFchull <- c(P56Control.PIFchull, P56Control.PIFchull[1])
P56Control.PIFpolygon <- P56Control.PIFdata[P56Control.PIFchull,]
P56Control.PIFarea <- Polygon(P56Control.PIFpolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$PIF[1:P56Control.PIFsize-1], P56Control.Pool$PIF[2:P56Control.PIFsize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$PIF[1:P56Control.PIFsize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 ControlHyper PIF Baseline
P21ControlHyper.PIFsize <- length(P21ControlHyper.Pool$PIF)
P21ControlHyper.PIFdata <- c(P21ControlHyper.Pool$PIF[1:P21ControlHyper.PIFsize-1],P21ControlHyper.Pool$PIF[2:P21ControlHyper.PIFsize])
P21ControlHyper.PIFdata <- matrix(P21ControlHyper.PIFdata, nrow=P21ControlHyper.PIFsize-1)
P21ControlHyper.PIFchull <- chull(P21ControlHyper.PIFdata)
P21ControlHyper.PIFchull <- c(P21ControlHyper.PIFchull, P21ControlHyper.PIFchull[1])
P21ControlHyper.PIFpolygon <- P21ControlHyper.PIFdata[P21ControlHyper.PIFchull,]
P21ControlHyper.PIFarea <- Polygon(P21ControlHyper.PIFpolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$PIF[1:P21ControlHyper.PIFsize-1], P21ControlHyper.Pool$PIF[2:P21ControlHyper.PIFsize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$PIF[1:P21ControlHyper.PIFsize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 ControlHyper PIF Baseline
P56ControlHyper.PIFsize <- length(P56ControlHyper.Pool$PIF)
P56ControlHyper.PIFdata <- c(P56ControlHyper.Pool$PIF[1:P56ControlHyper.PIFsize-1],P56ControlHyper.Pool$PIF[2:P56ControlHyper.PIFsize])
P56ControlHyper.PIFdata <- matrix(P56ControlHyper.PIFdata, nrow=P56ControlHyper.PIFsize-1)
P56ControlHyper.PIFchull <- chull(P56ControlHyper.PIFdata)
P56ControlHyper.PIFchull <- c(P56ControlHyper.PIFchull, P56ControlHyper.PIFchull[1])
P56ControlHyper.PIFpolygon <- P56ControlHyper.PIFdata[P56ControlHyper.PIFchull,]
P56ControlHyper.PIFarea <- Polygon(P56ControlHyper.PIFpolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$PIF[1:P56ControlHyper.PIFsize-1], P56ControlHyper.Pool$PIF[2:P56ControlHyper.PIFsize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$PIF[1:P56ControlHyper.PIFsize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

PIFarea.table <- c(P1Control.PIFarea, P21Control.PIFarea, P56Control.PIFarea, P21ControlHyper.PIFarea, P56ControlHyper.PIFarea)
PIFellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea,P21ControlHyper.ellarea, P56ControlHyper.ellarea)
PIFcenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#PEF
#P1 Control PEF Baseline
P1Control.PEFsize <- length(P1Control.Pool$PEF)
P1Control.PEFdata <- c(P1Control.Pool$PEF[1:P1Control.PEFsize-1],P1Control.Pool$PEF[2:P1Control.PEFsize])
P1Control.PEFdata <- matrix(P1Control.PEFdata, nrow=P1Control.PEFsize-1)
P1Control.PEFchull <- chull(P1Control.PEFdata)
P1Control.PEFchull <- c(P1Control.PEFchull, P1Control.PEFchull[1])
P1Control.PEFpolygon <- P1Control.PEFdata[P1Control.PEFchull,]
P1Control.PEFarea <- Polygon(P1Control.PEFpolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$PEF[1:P1Control.PEFsize-1], P1Control.Pool$PEF[2:P1Control.PEFsize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$PEF[1:P1Control.PEFsize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control PEF Baseline
P21Control.PEFsize <- length(P21Control.Pool$PEF)
P21Control.PEFdata <- c(P21Control.Pool$PEF[1:P21Control.PEFsize-1],P21Control.Pool$PEF[2:P21Control.PEFsize])
P21Control.PEFdata <- matrix(P21Control.PEFdata, nrow=P21Control.PEFsize-1)
P21Control.PEFchull <- chull(P21Control.PEFdata)
P21Control.PEFchull <- c(P21Control.PEFchull, P21Control.PEFchull[1])
P21Control.PEFpolygon <- P21Control.PEFdata[P21Control.PEFchull,]
P21Control.PEFarea <- Polygon(P21Control.PEFpolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$PEF[1:P21Control.PEFsize-1], P21Control.Pool$PEF[2:P21Control.PEFsize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$PEF[1:P21Control.PEFsize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control PEF Baseline
P56Control.PEFsize <- length(P56Control.Pool$PEF)
P56Control.PEFdata <- c(P56Control.Pool$PEF[1:P56Control.PEFsize-1],P56Control.Pool$PEF[2:P56Control.PEFsize])
P56Control.PEFdata <- matrix(P56Control.PEFdata, nrow=P56Control.PEFsize-1)
P56Control.PEFchull <- chull(P56Control.PEFdata)
P56Control.PEFchull <- c(P56Control.PEFchull, P56Control.PEFchull[1])
P56Control.PEFpolygon <- P56Control.PEFdata[P56Control.PEFchull,]
P56Control.PEFarea <- Polygon(P56Control.PEFpolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$PEF[1:P56Control.PEFsize-1], P56Control.Pool$PEF[2:P56Control.PEFsize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$PEF[1:P56Control.PEFsize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 ControlHyper PEF Baseline
P21ControlHyper.PEFsize <- length(P21ControlHyper.Pool$PEF)
P21ControlHyper.PEFdata <- c(P21ControlHyper.Pool$PEF[1:P21ControlHyper.PEFsize-1],P21ControlHyper.Pool$PEF[2:P21ControlHyper.PEFsize])
P21ControlHyper.PEFdata <- matrix(P21ControlHyper.PEFdata, nrow=P21ControlHyper.PEFsize-1)
P21ControlHyper.PEFchull <- chull(P21ControlHyper.PEFdata)
P21ControlHyper.PEFchull <- c(P21ControlHyper.PEFchull, P21ControlHyper.PEFchull[1])
P21ControlHyper.PEFpolygon <- P21ControlHyper.PEFdata[P21ControlHyper.PEFchull,]
P21ControlHyper.PEFarea <- Polygon(P21ControlHyper.PEFpolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$PEF[1:P21ControlHyper.PEFsize-1], P21ControlHyper.Pool$PEF[2:P21ControlHyper.PEFsize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$PEF[1:P21ControlHyper.PEFsize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 ControlHyper PEF Baseline
P56ControlHyper.PEFsize <- length(P56ControlHyper.Pool$PEF)
P56ControlHyper.PEFdata <- c(P56ControlHyper.Pool$PEF[1:P56ControlHyper.PEFsize-1],P56ControlHyper.Pool$PEF[2:P56ControlHyper.PEFsize])
P56ControlHyper.PEFdata <- matrix(P56ControlHyper.PEFdata, nrow=P56ControlHyper.PEFsize-1)
P56ControlHyper.PEFchull <- chull(P56ControlHyper.PEFdata)
P56ControlHyper.PEFchull <- c(P56ControlHyper.PEFchull, P56ControlHyper.PEFchull[1])
P56ControlHyper.PEFpolygon <- P56ControlHyper.PEFdata[P56ControlHyper.PEFchull,]
P56ControlHyper.PEFarea <- Polygon(P56ControlHyper.PEFpolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$PEF[1:P56ControlHyper.PEFsize-1], P56ControlHyper.Pool$PEF[2:P56ControlHyper.PEFsize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$PEF[1:P56ControlHyper.PEFsize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

PEFarea.table <- c(P1Control.PEFarea, P21Control.PEFarea, P56Control.PEFarea, P21ControlHyper.PEFarea, P56ControlHyper.PEFarea)
PEFellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea,P21ControlHyper.ellarea, P56ControlHyper.ellarea)
PEFcenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#Ti
#P1 Control Ti Baseline
P1Control.Tisize <- length(P1Control.Pool$Ti)
P1Control.Tidata <- c(P1Control.Pool$Ti[1:P1Control.Tisize-1],P1Control.Pool$Ti[2:P1Control.Tisize])
P1Control.Tidata <- matrix(P1Control.Tidata, nrow=P1Control.Tisize-1)
P1Control.Tichull <- chull(P1Control.Tidata)
P1Control.Tichull <- c(P1Control.Tichull, P1Control.Tichull[1])
P1Control.Tipolygon <- P1Control.Tidata[P1Control.Tichull,]
P1Control.Tiarea <- Polygon(P1Control.Tipolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$Ti[1:P1Control.Tisize-1], P1Control.Pool$Ti[2:P1Control.Tisize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$Ti[1:P1Control.Tisize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control Ti Baseline
P21Control.Tisize <- length(P21Control.Pool$Ti)
P21Control.Tidata <- c(P21Control.Pool$Ti[1:P21Control.Tisize-1],P21Control.Pool$Ti[2:P21Control.Tisize])
P21Control.Tidata <- matrix(P21Control.Tidata, nrow=P21Control.Tisize-1)
P21Control.Tichull <- chull(P21Control.Tidata)
P21Control.Tichull <- c(P21Control.Tichull, P21Control.Tichull[1])
P21Control.Tipolygon <- P21Control.Tidata[P21Control.Tichull,]
P21Control.Tiarea <- Polygon(P21Control.Tipolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$Ti[1:P21Control.Tisize-1], P21Control.Pool$Ti[2:P21Control.Tisize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$Ti[1:P21Control.Tisize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control Ti Baseline
P56Control.Tisize <- length(P56Control.Pool$Ti)
P56Control.Tidata <- c(P56Control.Pool$Ti[1:P56Control.Tisize-1],P56Control.Pool$Ti[2:P56Control.Tisize])
P56Control.Tidata <- matrix(P56Control.Tidata, nrow=P56Control.Tisize-1)
P56Control.Tichull <- chull(P56Control.Tidata)
P56Control.Tichull <- c(P56Control.Tichull, P56Control.Tichull[1])
P56Control.Tipolygon <- P56Control.Tidata[P56Control.Tichull,]
P56Control.Tiarea <- Polygon(P56Control.Tipolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$Ti[1:P56Control.Tisize-1], P56Control.Pool$Ti[2:P56Control.Tisize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$Ti[1:P56Control.Tisize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 ControlHyper Ti Baseline
P21ControlHyper.Tisize <- length(P21ControlHyper.Pool$Ti)
P21ControlHyper.Tidata <- c(P21ControlHyper.Pool$Ti[1:P21ControlHyper.Tisize-1],P21ControlHyper.Pool$Ti[2:P21ControlHyper.Tisize])
P21ControlHyper.Tidata <- matrix(P21ControlHyper.Tidata, nrow=P21ControlHyper.Tisize-1)
P21ControlHyper.Tichull <- chull(P21ControlHyper.Tidata)
P21ControlHyper.Tichull <- c(P21ControlHyper.Tichull, P21ControlHyper.Tichull[1])
P21ControlHyper.Tipolygon <- P21ControlHyper.Tidata[P21ControlHyper.Tichull,]
P21ControlHyper.Tiarea <- Polygon(P21ControlHyper.Tipolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$Ti[1:P21ControlHyper.Tisize-1], P21ControlHyper.Pool$Ti[2:P21ControlHyper.Tisize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$Ti[1:P21ControlHyper.Tisize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 ControlHyper Ti Baseline
P56ControlHyper.Tisize <- length(P56ControlHyper.Pool$Ti)
P56ControlHyper.Tidata <- c(P56ControlHyper.Pool$Ti[1:P56ControlHyper.Tisize-1],P56ControlHyper.Pool$Ti[2:P56ControlHyper.Tisize])
P56ControlHyper.Tidata <- matrix(P56ControlHyper.Tidata, nrow=P56ControlHyper.Tisize-1)
P56ControlHyper.Tichull <- chull(P56ControlHyper.Tidata)
P56ControlHyper.Tichull <- c(P56ControlHyper.Tichull, P56ControlHyper.Tichull[1])
P56ControlHyper.Tipolygon <- P56ControlHyper.Tidata[P56ControlHyper.Tichull,]
P56ControlHyper.Tiarea <- Polygon(P56ControlHyper.Tipolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$Ti[1:P56ControlHyper.Tisize-1], P56ControlHyper.Pool$Ti[2:P56ControlHyper.Tisize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$Ti[1:P56ControlHyper.Tisize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

Tiarea.table <- c(P1Control.Tiarea, P21Control.Tiarea, P56Control.Tiarea, P21ControlHyper.Tiarea, P56ControlHyper.Tiarea)
Tiellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea,P21ControlHyper.ellarea, P56ControlHyper.ellarea)
Ticenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#Te
#P1 Control Te Baseline
P1Control.Tesize <- length(P1Control.Pool$Te)
P1Control.Tedata <- c(P1Control.Pool$Te[1:P1Control.Tesize-1],P1Control.Pool$Te[2:P1Control.Tesize])
P1Control.Tedata <- matrix(P1Control.Tedata, nrow=P1Control.Tesize-1)
P1Control.Techull <- chull(P1Control.Tedata)
P1Control.Techull <- c(P1Control.Techull, P1Control.Techull[1])
P1Control.Tepolygon <- P1Control.Tedata[P1Control.Techull,]
P1Control.Tearea <- Polygon(P1Control.Tepolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$Te[1:P1Control.Tesize-1], P1Control.Pool$Te[2:P1Control.Tesize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$Te[1:P1Control.Tesize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control Te Baseline
P21Control.Tesize <- length(P21Control.Pool$Te)
P21Control.Tedata <- c(P21Control.Pool$Te[1:P21Control.Tesize-1],P21Control.Pool$Te[2:P21Control.Tesize])
P21Control.Tedata <- matrix(P21Control.Tedata, nrow=P21Control.Tesize-1)
P21Control.Techull <- chull(P21Control.Tedata)
P21Control.Techull <- c(P21Control.Techull, P21Control.Techull[1])
P21Control.Tepolygon <- P21Control.Tedata[P21Control.Techull,]
P21Control.Tearea <- Polygon(P21Control.Tepolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$Te[1:P21Control.Tesize-1], P21Control.Pool$Te[2:P21Control.Tesize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$Te[1:P21Control.Tesize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control Te Baseline
P56Control.Tesize <- length(P56Control.Pool$Te)
P56Control.Tedata <- c(P56Control.Pool$Te[1:P56Control.Tesize-1],P56Control.Pool$Te[2:P56Control.Tesize])
P56Control.Tedata <- matrix(P56Control.Tedata, nrow=P56Control.Tesize-1)
P56Control.Techull <- chull(P56Control.Tedata)
P56Control.Techull <- c(P56Control.Techull, P56Control.Techull[1])
P56Control.Tepolygon <- P56Control.Tedata[P56Control.Techull,]
P56Control.Tearea <- Polygon(P56Control.Tepolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$Te[1:P56Control.Tesize-1], P56Control.Pool$Te[2:P56Control.Tesize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$Te[1:P56Control.Tesize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 ControlHyper Te Baseline
P21ControlHyper.Tesize <- length(P21ControlHyper.Pool$Te)
P21ControlHyper.Tedata <- c(P21ControlHyper.Pool$Te[1:P21ControlHyper.Tesize-1],P21ControlHyper.Pool$Te[2:P21ControlHyper.Tesize])
P21ControlHyper.Tedata <- matrix(P21ControlHyper.Tedata, nrow=P21ControlHyper.Tesize-1)
P21ControlHyper.Techull <- chull(P21ControlHyper.Tedata)
P21ControlHyper.Techull <- c(P21ControlHyper.Techull, P21ControlHyper.Techull[1])
P21ControlHyper.Tepolygon <- P21ControlHyper.Tedata[P21ControlHyper.Techull,]
P21ControlHyper.Tearea <- Polygon(P21ControlHyper.Tepolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$Te[1:P21ControlHyper.Tesize-1], P21ControlHyper.Pool$Te[2:P21ControlHyper.Tesize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$Te[1:P21ControlHyper.Tesize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 ControlHyper Te Baseline
P56ControlHyper.Tesize <- length(P56ControlHyper.Pool$Te)
P56ControlHyper.Tedata <- c(P56ControlHyper.Pool$Te[1:P56ControlHyper.Tesize-1],P56ControlHyper.Pool$Te[2:P56ControlHyper.Tesize])
P56ControlHyper.Tedata <- matrix(P56ControlHyper.Tedata, nrow=P56ControlHyper.Tesize-1)
P56ControlHyper.Techull <- chull(P56ControlHyper.Tedata)
P56ControlHyper.Techull <- c(P56ControlHyper.Techull, P56ControlHyper.Techull[1])
P56ControlHyper.Tepolygon <- P56ControlHyper.Tedata[P56ControlHyper.Techull,]
P56ControlHyper.Tearea <- Polygon(P56ControlHyper.Tepolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$Te[1:P56ControlHyper.Tesize-1], P56ControlHyper.Pool$Te[2:P56ControlHyper.Tesize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$Te[1:P56ControlHyper.Tesize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

Tearea.table <- c(P1Control.Tearea, P21Control.Tearea, P56Control.Tearea, P21ControlHyper.Tearea, P56ControlHyper.Tearea)
Teellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea,P21ControlHyper.ellarea, P56ControlHyper.ellarea)
Tecenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#Tr
#P1 Control Tr Baseline
P1Control.Trsize <- length(P1Control.Pool$Tr)
P1Control.Trdata <- c(P1Control.Pool$Tr[1:P1Control.Trsize-1],P1Control.Pool$Tr[2:P1Control.Trsize])
P1Control.Trdata <- matrix(P1Control.Trdata, nrow=P1Control.Trsize-1)
P1Control.Trchull <- chull(P1Control.Trdata)
P1Control.Trchull <- c(P1Control.Trchull, P1Control.Trchull[1])
P1Control.Trpolygon <- P1Control.Trdata[P1Control.Trchull,]
P1Control.Trarea <- Polygon(P1Control.Trpolygon, hole = FALSE)@area
ellP1Control.info <- cov.wt(cbind(P1Control.Pool$Tr[1:P1Control.Trsize-1], P1Control.Pool$Tr[2:P1Control.Trsize]))
eigenP1Control.info <- eigen(ellP1Control.info$cov)
P1Control.lengths <- sqrt(eigenP1Control.info$values*2*qf(0.68,2,length(P1Control.Pool$Tr[1:P1Control.Trsize-1])-1))
P1Control.ellarea <- pi*P1Control.lengths[1]*P1Control.lengths[2]
P1Control.center <- ellP1Control.info$center

#P21 Control Tr Baseline
P21Control.Trsize <- length(P21Control.Pool$Tr)
P21Control.Trdata <- c(P21Control.Pool$Tr[1:P21Control.Trsize-1],P21Control.Pool$Tr[2:P21Control.Trsize])
P21Control.Trdata <- matrix(P21Control.Trdata, nrow=P21Control.Trsize-1)
P21Control.Trchull <- chull(P21Control.Trdata)
P21Control.Trchull <- c(P21Control.Trchull, P21Control.Trchull[1])
P21Control.Trpolygon <- P21Control.Trdata[P21Control.Trchull,]
P21Control.Trarea <- Polygon(P21Control.Trpolygon, hole = FALSE)@area
ellP21Control.info <- cov.wt(cbind(P21Control.Pool$Tr[1:P21Control.Trsize-1], P21Control.Pool$Tr[2:P21Control.Trsize]))
eigenP21Control.info <- eigen(ellP21Control.info$cov)
P21Control.lengths <- sqrt(eigenP21Control.info$values*2*qf(0.68,2,length(P21Control.Pool$Tr[1:P21Control.Trsize-1])-1))
P21Control.ellarea <- pi*P21Control.lengths[1]*P21Control.lengths[2]
P21Control.center <- ellP21Control.info$center

#P56 Control Tr Baseline
P56Control.Trsize <- length(P56Control.Pool$Tr)
P56Control.Trdata <- c(P56Control.Pool$Tr[1:P56Control.Trsize-1],P56Control.Pool$Tr[2:P56Control.Trsize])
P56Control.Trdata <- matrix(P56Control.Trdata, nrow=P56Control.Trsize-1)
P56Control.Trchull <- chull(P56Control.Trdata)
P56Control.Trchull <- c(P56Control.Trchull, P56Control.Trchull[1])
P56Control.Trpolygon <- P56Control.Trdata[P56Control.Trchull,]
P56Control.Trarea <- Polygon(P56Control.Trpolygon, hole = FALSE)@area
ellP56Control.info <- cov.wt(cbind(P56Control.Pool$Tr[1:P56Control.Trsize-1], P56Control.Pool$Tr[2:P56Control.Trsize]))
eigenP56Control.info <- eigen(ellP56Control.info$cov)
P56Control.lengths <- sqrt(eigenP56Control.info$values*2*qf(0.68,2,length(P56Control.Pool$Tr[1:P56Control.Trsize-1])-1))
P56Control.ellarea <- pi*P56Control.lengths[1]*P56Control.lengths[2]
P56Control.center <- ellP56Control.info$center

#P21 ControlHyper Tr Baseline
P21ControlHyper.Trsize <- length(P21ControlHyper.Pool$Tr)
P21ControlHyper.Trdata <- c(P21ControlHyper.Pool$Tr[1:P21ControlHyper.Trsize-1],P21ControlHyper.Pool$Tr[2:P21ControlHyper.Trsize])
P21ControlHyper.Trdata <- matrix(P21ControlHyper.Trdata, nrow=P21ControlHyper.Trsize-1)
P21ControlHyper.Trchull <- chull(P21ControlHyper.Trdata)
P21ControlHyper.Trchull <- c(P21ControlHyper.Trchull, P21ControlHyper.Trchull[1])
P21ControlHyper.Trpolygon <- P21ControlHyper.Trdata[P21ControlHyper.Trchull,]
P21ControlHyper.Trarea <- Polygon(P21ControlHyper.Trpolygon, hole = FALSE)@area
ellP21ControlHyper.info <- cov.wt(cbind(P21ControlHyper.Pool$Tr[1:P21ControlHyper.Trsize-1], P21ControlHyper.Pool$Tr[2:P21ControlHyper.Trsize]))
eigenP21ControlHyper.info <- eigen(ellP21ControlHyper.info$cov)
P21ControlHyper.lengths <- sqrt(eigenP21ControlHyper.info$values*2*qf(0.68,2,length(P21ControlHyper.Pool$Tr[1:P21ControlHyper.Trsize-1])-1))
P21ControlHyper.ellarea <- pi*P21ControlHyper.lengths[1]*P21ControlHyper.lengths[2]
P21ControlHyper.center <- ellP21ControlHyper.info$center

#P56 ControlHyper Tr Baseline
P56ControlHyper.Trsize <- length(P56ControlHyper.Pool$Tr)
P56ControlHyper.Trdata <- c(P56ControlHyper.Pool$Tr[1:P56ControlHyper.Trsize-1],P56ControlHyper.Pool$Tr[2:P56ControlHyper.Trsize])
P56ControlHyper.Trdata <- matrix(P56ControlHyper.Trdata, nrow=P56ControlHyper.Trsize-1)
P56ControlHyper.Trchull <- chull(P56ControlHyper.Trdata)
P56ControlHyper.Trchull <- c(P56ControlHyper.Trchull, P56ControlHyper.Trchull[1])
P56ControlHyper.Trpolygon <- P56ControlHyper.Trdata[P56ControlHyper.Trchull,]
P56ControlHyper.Trarea <- Polygon(P56ControlHyper.Trpolygon, hole = FALSE)@area
ellP56ControlHyper.info <- cov.wt(cbind(P56ControlHyper.Pool$Tr[1:P56ControlHyper.Trsize-1], P56ControlHyper.Pool$Tr[2:P56ControlHyper.Trsize]))
eigenP56ControlHyper.info <- eigen(ellP56ControlHyper.info$cov)
P56ControlHyper.lengths <- sqrt(eigenP56ControlHyper.info$values*2*qf(0.68,2,length(P56ControlHyper.Pool$Tr[1:P56ControlHyper.Trsize-1])-1))
P56ControlHyper.ellarea <- pi*P56ControlHyper.lengths[1]*P56ControlHyper.lengths[2]
P56ControlHyper.center <- ellP56ControlHyper.info$center

Trarea.table <- c(P1Control.Trarea, P21Control.Trarea, P56Control.Trarea, P21ControlHyper.Trarea, P56ControlHyper.Trarea)
Trellipsearea.table <- c(P1Control.ellarea,P21Control.ellarea,P56Control.ellarea, P21ControlHyper.ellarea, P56ControlHyper.ellarea)
Trcenter.table <- rbind(P1Control.center,P21Control.center,P56Control.center,P21ControlHyper.center,P56ControlHyper.center)

#Poincare Figures
library(grid)
library(gridExtra)
blank <- grid.rect(gp=gpar(col="white"))
library(ggpubr)

P1Control.Pool[,8] <-  c("Baseline")
P1Control.Pool[,9] <-  c("P1")
P1Control.areaframe[,15] <- c("Baseline")
P1Control.areaframe[,16] <- c("P1")

P21Control.Pool[,8] <-  c("Baseline")
P21Control.Pool[,9] <-  c("P21")
P21Control.areaframe[,15] <- c("Baseline")
P21Control.areaframe[,16] <- c("P21")

P56Control.Pool[,8] <-  c("Baseline")
P56Control.Pool[,9] <-  c("P56")
P56Control.areaframe[,15] <- c("Baseline")
P56Control.areaframe[,16] <- c("P56")

P21ControlHyper.Pool[,8] <-  c("Hypercapnia")
P21ControlHyper.Pool[,9] <-  c("P21")
P21ControlHyper.areaframe[,15] <- c("Hypercapnia")
P21ControlHyper.areaframe[,16] <- c("P21")

P56ControlHyper.Pool[,8] <-  c("Hypercapnia")
P56ControlHyper.Pool[,9] <-  c("P56")
P56ControlHyper.areaframe[,15] <- c("Hypercapnia")
P56ControlHyper.areaframe[,16] <- c("P56")

Data.Pool <- rbind(P1Control.Pool,P21Control.Pool,P56Control.Pool,P21ControlHyper.Pool,P56ControlHyper.Pool)
names(Data.Pool)[8] <- c("Breathing")
names(Data.Pool)[9] <- c("Age")
Area.Pool <- rbind(P1Control.areaframe, P21Control.areaframe, P56Control.areaframe, P21ControlHyper.areaframe, P56ControlHyper.areaframe)
names(Area.Pool)[15] <- c("Breathing")
names(Area.Pool)[16] <- c("Age")


#Frequency
mean.fplot <- ggplot(Data.Pool, aes(x = Breathing, y = f, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.fplot <- ggplot(Area.Pool, aes(x = Breathing, y = f.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.fplot <- ggplot(Area.Pool, aes(x = Breathing, y = f.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control f Baseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.ffigure <- as.data.frame(cbind(P1Control.Pool$f[1:P1Control.fsize-1],P1Control.Pool$f[2:P1Control.fsize]))
hull <- P1Control.ffigure %>%
  slice(chull(V1,V2))
P1Control.ffigure <- as.data.frame(cbind(P1Control.Pool$f[1:P1Control.fsize-1],P1Control.Pool$f[2:P1Control.fsize]))
fplot1 <- ggplot(P1Control.ffigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Frequency N") +
  ylab("Frequency N+1") +
  ggtitle("P1 Baseline Frequency") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)

#P21 Control f Baseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.ffigure <- as.data.frame(cbind(P21Control.Pool$f[1:P21Control.fsize-1],P21Control.Pool$f[2:P21Control.fsize]))
hull <- P21Control.ffigure %>%
  slice(chull(V1,V2))
P21Control.ffigure <- as.data.frame(cbind(P21Control.Pool$f[1:P21Control.fsize-1],P21Control.Pool$f[2:P21Control.fsize]))
fplot2 <- ggplot(P21Control.ffigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Frequency N") +
  ylab("Frequency N+1") +
  ggtitle("P21 Baseline Frequency") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)


#P56 Control f Baseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.ffigure <- as.data.frame(cbind(P56Control.Pool$f[1:P56Control.fsize-1],P56Control.Pool$f[2:P56Control.fsize]))
hull <- P56Control.ffigure %>%
  slice(chull(V1,V2))
P56Control.ffigure <- as.data.frame(cbind(P56Control.Pool$f[1:P56Control.fsize-1],P56Control.Pool$f[2:P56Control.fsize]))
fplot3 <- ggplot(P56Control.ffigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Frequency N") +
  ylab("Frequency N+1") +
  ggtitle("P56 Baseline Frequency") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)


#P21 Control f Hypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.ffigure <- as.data.frame(cbind(P21ControlHyper.Pool$f[1:P21ControlHyper.fsize-1],P21ControlHyper.Pool$f[2:P21ControlHyper.fsize]))
hull <- P21ControlHyper.ffigure %>%
  slice(chull(V1,V2))
P21ControlHyper.ffigure <- as.data.frame(cbind(P21ControlHyper.Pool$f[1:P21ControlHyper.fsize-1],P21ControlHyper.Pool$f[2:P21ControlHyper.fsize]))
fplot4 <- ggplot(P21ControlHyper.ffigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Frequency N") +
  ylab("Frequency N+1") +
  ggtitle("P21 Hypercapnia Frequency") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)


#P56 Control f Hypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.ffigure <- as.data.frame(cbind(P56ControlHyper.Pool$f[1:P56ControlHyper.fsize-1],P56ControlHyper.Pool$f[2:P56ControlHyper.fsize]))
hull <- P56ControlHyper.ffigure %>%
  slice(chull(V1,V2))
P56ControlHyper.ffigure <- as.data.frame(cbind(P56ControlHyper.Pool$f[1:P56ControlHyper.fsize-1],P56ControlHyper.Pool$f[2:P56ControlHyper.fsize]))
fplot5 <- ggplot(P56ControlHyper.ffigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Frequency N") +
  ylab("Frequency N+1") +
  ggtitle("P56 Hypercapnia Frequency") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)



ggarrange(mean.fplot, ellipse.fplot, polygon.fplot, fplot1, fplot2, fplot3, blank, fplot4, fplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))






#Tidal Volume
mean.TVplot <- ggplot(Data.Pool, aes(x = Breathing, y = TV, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.TVplot <- ggplot(Area.Pool, aes(x = Breathing, y = TV.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.TVplot <- ggplot(Area.Pool, aes(x = Breathing, y = TV.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control TV Baseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.TVfigure <- as.data.frame(cbind(P1Control.Pool$TV[1:P1Control.TVsize-1],P1Control.Pool$TV[2:P1Control.TVsize]))
hull <- P1Control.TVfigure %>%
  slice(chull(V1,V2))
P1Control.TVfigure <- as.data.frame(cbind(P1Control.Pool$TV[1:P1Control.TVsize-1],P1Control.Pool$TV[2:P1Control.TVsize]))
TVplot1 <- ggplot(P1Control.TVfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tidal Volume N") +
  ylab("Tidal Volume N+1") +
  ggtitle("P1 Baseline Tidal Volume") + 
  theme_bw()+ 
  xlim(0,40)+
  ylim(0,40)

#P21 Control TV Baseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.TVfigure <- as.data.frame(cbind(P21Control.Pool$TV[1:P21Control.TVsize-1],P21Control.Pool$TV[2:P21Control.TVsize]))
hull <- P21Control.TVfigure %>%
  slice(chull(V1,V2))
P21Control.TVfigure <- as.data.frame(cbind(P21Control.Pool$TV[1:P21Control.TVsize-1],P21Control.Pool$TV[2:P21Control.TVsize]))
TVplot2 <- ggplot(P21Control.TVfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tidal Volume N") +
  ylab("Tidal Volume N+1") +
  ggtitle("P21 Baseline Tidal Volume") + 
  theme_bw()+ 
  xlim(0,40)+
  ylim(0,40)

#P56 Control TV Baseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.TVfigure <- as.data.frame(cbind(P56Control.Pool$TV[1:P56Control.TVsize-1],P56Control.Pool$TV[2:P56Control.TVsize]))
hull <- P56Control.TVfigure %>%
  slice(chull(V1,V2))
P56Control.TVfigure <- as.data.frame(cbind(P56Control.Pool$TV[1:P56Control.TVsize-1],P56Control.Pool$TV[2:P56Control.TVsize]))
TVplot3 <- ggplot(P56Control.TVfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tidal Volume N") +
  ylab("Tidal Volume N+1") +
  ggtitle("P56 Baseline Tidal Volume") + 
  theme_bw()+ 
  xlim(0,40)+
  ylim(0,40)

#P21 Control TV Hypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.TVfigure <- as.data.frame(cbind(P21ControlHyper.Pool$TV[1:P21ControlHyper.TVsize-1],P21ControlHyper.Pool$TV[2:P21ControlHyper.TVsize]))
hull <- P21ControlHyper.TVfigure %>%
  slice(chull(V1,V2))
P21ControlHyper.TVfigure <- as.data.frame(cbind(P21ControlHyper.Pool$TV[1:P21ControlHyper.TVsize-1],P21ControlHyper.Pool$TV[2:P21ControlHyper.TVsize]))
TVplot4 <- ggplot(P21ControlHyper.TVfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tidal Volume N") +
  ylab("Tidal Volume N+1") +
  ggtitle("P21 Hypercapnia Tidal Volume") + 
  theme_bw()+ 
  xlim(0,40)+
  ylim(0,40)

#P56 Control TV Hypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.TVfigure <- as.data.frame(cbind(P56ControlHyper.Pool$TV[1:P56ControlHyper.TVsize-1],P56ControlHyper.Pool$TV[2:P56ControlHyper.TVsize]))
hull <- P56ControlHyper.TVfigure %>%
  slice(chull(V1,V2))
P56ControlHyper.TVfigure <- as.data.frame(cbind(P56ControlHyper.Pool$TV[1:P56ControlHyper.TVsize-1],P56ControlHyper.Pool$TV[2:P56ControlHyper.TVsize]))
TVplot5 <- ggplot(P56ControlHyper.TVfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tidal Volume N") +
  ylab("Tidal Volume N+1") +
  ggtitle("P56 Hypercapnia Tidal Volume") + 
  theme_bw()+ 
  xlim(0,40)+
  ylim(0,40)


ggarrange(mean.TVplot, ellipse.TVplot, polygon.TVplot, TVplot1, TVplot2, TVplot3, blank, TVplot4, TVplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))







#Peak Inspiratory Flow
mean.PIFplot <- ggplot(Data.Pool, aes(x = Breathing, y = PIF, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.PIFplot <- ggplot(Area.Pool, aes(x = Breathing, y = PIF.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.PIFplot <- ggplot(Area.Pool, aes(x = Breathing, y = PIF.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control PIFBaseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.PIFfigure <- as.data.frame(cbind(P1Control.Pool$PIF[1:P1Control.PIFsize-1],P1Control.Pool$PIF[2:P1Control.PIFsize]))
hull <- P1Control.PIFfigure %>%
  slice(chull(V1,V2))
P1Control.PIFfigure <- as.data.frame(cbind(P1Control.Pool$PIF[1:P1Control.PIFsize-1],P1Control.Pool$PIF[2:P1Control.PIFsize]))
PIFplot1 <- ggplot(P1Control.PIFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PIF N") +
  ylab("PIF N+1") +
  ggtitle("P1 Baseline PIF") + 
  theme_bw()+ 
  xlim(0,1100)+
  ylim(0,1100)

#P21 Control PIFBaseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.PIFfigure <- as.data.frame(cbind(P21Control.Pool$PIF[1:P21Control.PIFsize-1],P21Control.Pool$PIF[2:P21Control.PIFsize]))
hull <- P21Control.PIFfigure %>%
  slice(chull(V1,V2))
P21Control.PIFfigure <- as.data.frame(cbind(P21Control.Pool$PIF[1:P21Control.PIFsize-1],P21Control.Pool$PIF[2:P21Control.PIFsize]))
PIFplot2 <- ggplot(P21Control.PIFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PIF N") +
  ylab("PIF N+1") +
  ggtitle("P21 Baseline PIF") + 
  theme_bw()+ 
  xlim(0,1100)+
  ylim(0,1100)

#P56 Control PIFBaseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.PIFfigure <- as.data.frame(cbind(P56Control.Pool$PIF[1:P56Control.PIFsize-1],P56Control.Pool$PIF[2:P56Control.PIFsize]))
hull <- P56Control.PIFfigure %>%
  slice(chull(V1,V2))
P56Control.PIFfigure <- as.data.frame(cbind(P56Control.Pool$PIF[1:P56Control.PIFsize-1],P56Control.Pool$PIF[2:P56Control.PIFsize]))
PIFplot3 <- ggplot(P56Control.PIFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PIF N") +
  ylab("PIF N+1") +
  ggtitle("P56 Baseline PIF") + 
  theme_bw()+ 
  xlim(0,1100)+
  ylim(0,1100)

#P21 Control PIFHypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.PIFfigure <- as.data.frame(cbind(P21ControlHyper.Pool$PIF[1:P21ControlHyper.PIFsize-1],P21ControlHyper.Pool$PIF[2:P21ControlHyper.PIFsize]))
hull <- P21ControlHyper.PIFfigure %>%
  slice(chull(V1,V2))
P21ControlHyper.PIFfigure <- as.data.frame(cbind(P21ControlHyper.Pool$PIF[1:P21ControlHyper.PIFsize-1],P21ControlHyper.Pool$PIF[2:P21ControlHyper.PIFsize]))
PIFplot4 <- ggplot(P21ControlHyper.PIFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PIF N") +
  ylab("PIF N+1") +
  ggtitle("P21 Hypercapnia PIF") + 
  theme_bw()+ 
  xlim(0,1100)+
  ylim(0,1100)

#P56 Control PIFHypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.PIFfigure <- as.data.frame(cbind(P56ControlHyper.Pool$PIF[1:P56ControlHyper.PIFsize-1],P56ControlHyper.Pool$PIF[2:P56ControlHyper.PIFsize]))
hull <- P56ControlHyper.PIFfigure %>%
  slice(chull(V1,V2))
P56ControlHyper.PIFfigure <- as.data.frame(cbind(P56ControlHyper.Pool$PIF[1:P56ControlHyper.PIFsize-1],P56ControlHyper.Pool$PIF[2:P56ControlHyper.PIFsize]))
PIFplot5 <- ggplot(P56ControlHyper.PIFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PIF N") +
  ylab("PIF N+1") +
  ggtitle("P56 Hypercapnia PIF") + 
  theme_bw()+ 
  xlim(0,1100)+
  ylim(0,1100)


ggarrange(mean.PIFplot, ellipse.PIFplot, polygon.PIFplot, PIFplot1, PIFplot2, PIFplot3, blank, PIFplot4, PIFplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))



#Peak Expiratory Flow
mean.PEFplot <- ggplot(Data.Pool, aes(x = Breathing, y = PEF, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.PEFplot <- ggplot(Area.Pool, aes(x = Breathing, y = PEF.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.PEFplot <- ggplot(Area.Pool, aes(x = Breathing, y = PEF.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control PEFBaseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.PEFfigure <- as.data.frame(cbind(P1Control.Pool$PEF[1:P1Control.PEFsize-1],P1Control.Pool$PEF[2:P1Control.PEFsize]))
hull <- P1Control.PEFfigure %>%
  slice(chull(V1,V2))
P1Control.PEFfigure <- as.data.frame(cbind(P1Control.Pool$PEF[1:P1Control.PEFsize-1],P1Control.Pool$PEF[2:P1Control.PEFsize]))
PEFplot1 <- ggplot(P1Control.PEFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PEF N") +
  ylab("PEF N+1") +
  ggtitle("P1 Baseline PEF") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)

#P21 Control PEFBaseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.PEFfigure <- as.data.frame(cbind(P21Control.Pool$PEF[1:P21Control.PEFsize-1],P21Control.Pool$PEF[2:P21Control.PEFsize]))
hull <- P21Control.PEFfigure %>%
  slice(chull(V1,V2))
P21Control.PEFfigure <- as.data.frame(cbind(P21Control.Pool$PEF[1:P21Control.PEFsize-1],P21Control.Pool$PEF[2:P21Control.PEFsize]))
PEFplot2 <- ggplot(P21Control.PEFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PEF N") +
  ylab("PEF N+1") +
  ggtitle("P21 Baseline PEF") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)

#P56 Control PEFBaseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.PEFfigure <- as.data.frame(cbind(P56Control.Pool$PEF[1:P56Control.PEFsize-1],P56Control.Pool$PEF[2:P56Control.PEFsize]))
hull <- P56Control.PEFfigure %>%
  slice(chull(V1,V2))
P56Control.PEFfigure <- as.data.frame(cbind(P56Control.Pool$PEF[1:P56Control.PEFsize-1],P56Control.Pool$PEF[2:P56Control.PEFsize]))
PEFplot3 <- ggplot(P56Control.PEFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PEF N") +
  ylab("PEF N+1") +
  ggtitle("P56 Baseline PEF") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)

#P21 Control PEFHypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.PEFfigure <- as.data.frame(cbind(P21ControlHyper.Pool$PEF[1:P21ControlHyper.PEFsize-1],P21ControlHyper.Pool$PEF[2:P21ControlHyper.PEFsize]))
hull <- P21ControlHyper.PEFfigure %>%
  slice(chull(V1,V2))
P21ControlHyper.PEFfigure <- as.data.frame(cbind(P21ControlHyper.Pool$PEF[1:P21ControlHyper.PEFsize-1],P21ControlHyper.Pool$PEF[2:P21ControlHyper.PEFsize]))
PEFplot4 <- ggplot(P21ControlHyper.PEFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PEF N") +
  ylab("PEF N+1") +
  ggtitle("P21 Hypercapnia PEF") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)

#P56 Control PEFHypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.PEFfigure <- as.data.frame(cbind(P56ControlHyper.Pool$PEF[1:P56ControlHyper.PEFsize-1],P56ControlHyper.Pool$PEF[2:P56ControlHyper.PEFsize]))
hull <- P56ControlHyper.PEFfigure %>%
  slice(chull(V1,V2))
P56ControlHyper.PEFfigure <- as.data.frame(cbind(P56ControlHyper.Pool$PEF[1:P56ControlHyper.PEFsize-1],P56ControlHyper.Pool$PEF[2:P56ControlHyper.PEFsize]))
PEFplot5 <- ggplot(P56ControlHyper.PEFfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("PEF N") +
  ylab("PEF N+1") +
  ggtitle("P56 Hypercapnia PEF") + 
  theme_bw()+ 
  xlim(0,800)+
  ylim(0,800)


ggarrange(mean.PEFplot, ellipse.PEFplot, polygon.PEFplot, PEFplot1, PEFplot2, PEFplot3, blank, PEFplot4, PEFplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))



#Ti
mean.Tiplot <- ggplot(Data.Pool, aes(x = Breathing, y = Ti, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.Tiplot <- ggplot(Area.Pool, aes(x = Breathing, y = Ti.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.Tiplot <- ggplot(Area.Pool, aes(x = Breathing, y = Ti.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control TiBaseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.Tifigure <- as.data.frame(cbind(P1Control.Pool$Ti[1:P1Control.Tisize-1],P1Control.Pool$Ti[2:P1Control.Tisize]))
hull <- P1Control.Tifigure %>%
  slice(chull(V1,V2))
P1Control.Tifigure <- as.data.frame(cbind(P1Control.Pool$Ti[1:P1Control.Tisize-1],P1Control.Pool$Ti[2:P1Control.Tisize]))
Tiplot1 <- ggplot(P1Control.Tifigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Ti N") +
  ylab("Ti N+1") +
  ggtitle("P1 Baseline Ti") + 
  theme_bw()+ 
  xlim(0,0.5)+
  ylim(0,0.5)

#P21 Control TiBaseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.Tifigure <- as.data.frame(cbind(P21Control.Pool$Ti[1:P21Control.Tisize-1],P21Control.Pool$Ti[2:P21Control.Tisize]))
hull <- P21Control.Tifigure %>%
  slice(chull(V1,V2))
P21Control.Tifigure <- as.data.frame(cbind(P21Control.Pool$Ti[1:P21Control.Tisize-1],P21Control.Pool$Ti[2:P21Control.Tisize]))
Tiplot2 <- ggplot(P21Control.Tifigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Ti N") +
  ylab("Ti N+1") +
  ggtitle("P21 Baseline Ti") + 
  theme_bw()+ 
  xlim(0,0.5)+
  ylim(0,0.5)

#P56 Control TiBaseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.Tifigure <- as.data.frame(cbind(P56Control.Pool$Ti[1:P56Control.Tisize-1],P56Control.Pool$Ti[2:P56Control.Tisize]))
hull <- P56Control.Tifigure %>%
  slice(chull(V1,V2))
P56Control.Tifigure <- as.data.frame(cbind(P56Control.Pool$Ti[1:P56Control.Tisize-1],P56Control.Pool$Ti[2:P56Control.Tisize]))
Tiplot3 <- ggplot(P56Control.Tifigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Ti N") +
  ylab("Ti N+1") +
  ggtitle("P56 Baseline Ti") + 
  theme_bw()+ 
  xlim(0,0.5)+
  ylim(0,0.5)

#P21 Control TiHypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.Tifigure <- as.data.frame(cbind(P21ControlHyper.Pool$Ti[1:P21ControlHyper.Tisize-1],P21ControlHyper.Pool$Ti[2:P21ControlHyper.Tisize]))
hull <- P21ControlHyper.Tifigure %>%
  slice(chull(V1,V2))
P21ControlHyper.Tifigure <- as.data.frame(cbind(P21ControlHyper.Pool$Ti[1:P21ControlHyper.Tisize-1],P21ControlHyper.Pool$Ti[2:P21ControlHyper.Tisize]))
Tiplot4 <- ggplot(P21ControlHyper.Tifigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Ti N") +
  ylab("Ti N+1") +
  ggtitle("P21 Hypercapnia Ti") + 
  theme_bw()+ 
  xlim(0,0.5)+
  ylim(0,0.5)

#P56 Control TiHypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.Tifigure <- as.data.frame(cbind(P56ControlHyper.Pool$Ti[1:P56ControlHyper.Tisize-1],P56ControlHyper.Pool$Ti[2:P56ControlHyper.Tisize]))
hull <- P56ControlHyper.Tifigure %>%
  slice(chull(V1,V2))
P56ControlHyper.Tifigure <- as.data.frame(cbind(P56ControlHyper.Pool$Ti[1:P56ControlHyper.Tisize-1],P56ControlHyper.Pool$Ti[2:P56ControlHyper.Tisize]))
Tiplot5 <- ggplot(P56ControlHyper.Tifigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Ti N") +
  ylab("Ti N+1") +
  ggtitle("P56 Hypercapnia Ti") + 
  theme_bw()+ 
  xlim(0,0.5)+
  ylim(0,0.5)


ggarrange(mean.Tiplot, ellipse.Tiplot, polygon.Tiplot, Tiplot1, Tiplot2, Tiplot3, blank, Tiplot4, Tiplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))



#Te
mean.Teplot <- ggplot(Data.Pool, aes(x = Breathing, y = Te, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.Teplot <- ggplot(Area.Pool, aes(x = Breathing, y = Te.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.Teplot <- ggplot(Area.Pool, aes(x = Breathing, y = Te.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control TeBaseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.Tefigure <- as.data.frame(cbind(P1Control.Pool$Te[1:P1Control.Tesize-1],P1Control.Pool$Te[2:P1Control.Tesize]))
hull <- P1Control.Tefigure %>%
  slice(chull(V1,V2))
P1Control.Tefigure <- as.data.frame(cbind(P1Control.Pool$Te[1:P1Control.Tesize-1],P1Control.Pool$Te[2:P1Control.Tesize]))
Teplot1 <- ggplot(P1Control.Tefigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Te N") +
  ylab("Te N+1") +
  ggtitle("P1 Baseline Te") + 
  theme_bw()+ 
  xlim(0,1.1)+
  ylim(0,1.1)

#P21 Control TeBaseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.Tefigure <- as.data.frame(cbind(P21Control.Pool$Te[1:P21Control.Tesize-1],P21Control.Pool$Te[2:P21Control.Tesize]))
hull <- P21Control.Tefigure %>%
  slice(chull(V1,V2))
P21Control.Tefigure <- as.data.frame(cbind(P21Control.Pool$Te[1:P21Control.Tesize-1],P21Control.Pool$Te[2:P21Control.Tesize]))
Teplot2 <- ggplot(P21Control.Tefigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Te N") +
  ylab("Te N+1") +
  ggtitle("P21 Baseline Te") + 
  theme_bw()+ 
  xlim(0,1.1)+
  ylim(0,1.1)

#P56 Control TeBaseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.Tefigure <- as.data.frame(cbind(P56Control.Pool$Te[1:P56Control.Tesize-1],P56Control.Pool$Te[2:P56Control.Tesize]))
hull <- P56Control.Tefigure %>%
  slice(chull(V1,V2))
P56Control.Tefigure <- as.data.frame(cbind(P56Control.Pool$Te[1:P56Control.Tesize-1],P56Control.Pool$Te[2:P56Control.Tesize]))
Teplot3 <- ggplot(P56Control.Tefigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Te N") +
  ylab("Te N+1") +
  ggtitle("P56 Baseline Te") + 
  theme_bw()+ 
  xlim(0,1.1)+
  ylim(0,1.1)

#P21 Control TeHypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.Tefigure <- as.data.frame(cbind(P21ControlHyper.Pool$Te[1:P21ControlHyper.Tesize-1],P21ControlHyper.Pool$Te[2:P21ControlHyper.Tesize]))
hull <- P21ControlHyper.Tefigure %>%
  slice(chull(V1,V2))
P21ControlHyper.Tefigure <- as.data.frame(cbind(P21ControlHyper.Pool$Te[1:P21ControlHyper.Tesize-1],P21ControlHyper.Pool$Te[2:P21ControlHyper.Tesize]))
Teplot4 <- ggplot(P21ControlHyper.Tefigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Te N") +
  ylab("Te N+1") +
  ggtitle("P21 Hypercapnia Te") + 
  theme_bw()+ 
  xlim(0,1.1)+
  ylim(0,1.1)

#P56 Control TeHypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.Tefigure <- as.data.frame(cbind(P56ControlHyper.Pool$Te[1:P56ControlHyper.Tesize-1],P56ControlHyper.Pool$Te[2:P56ControlHyper.Tesize]))
hull <- P56ControlHyper.Tefigure %>%
  slice(chull(V1,V2))
P56ControlHyper.Tefigure <- as.data.frame(cbind(P56ControlHyper.Pool$Te[1:P56ControlHyper.Tesize-1],P56ControlHyper.Pool$Te[2:P56ControlHyper.Tesize]))
Teplot5 <- ggplot(P56ControlHyper.Tefigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Te N") +
  ylab("Te N+1") +
  ggtitle("P56 Hypercapnia Te") + 
  theme_bw()+ 
  xlim(0,1.1)+
  ylim(0,1.1)


ggarrange(mean.Teplot, ellipse.Teplot, polygon.Teplot, Teplot1, Teplot2, Teplot3, blank, Teplot4, Teplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))



#Tr
mean.Trplot <- ggplot(Data.Pool, aes(x = Breathing, y = Tr, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Mean") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

ellipse.Trplot <- ggplot(Area.Pool, aes(x = Breathing, y = Tr.Ellipse.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Ellipse Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

polygon.Trplot <- ggplot(Area.Pool, aes(x = Breathing, y = Tr.Polygon.Area, fill = Age)) +
  geom_boxplot() +
  facet_wrap(~ Age) +
  xlab("") + 
  ylab("Polygon Area") +
  theme_bw()  +
  theme(axis.text.x = element_text(angle = 90))

#P1 Control TrBaseline
P1Control.Pool <- as.data.frame(P1Control.Pool)
P1Control.Trfigure <- as.data.frame(cbind(P1Control.Pool$Tr[1:P1Control.Trsize-1],P1Control.Pool$Tr[2:P1Control.Trsize]))
hull <- P1Control.Trfigure %>%
  slice(chull(V1,V2))
P1Control.Trfigure <- as.data.frame(cbind(P1Control.Pool$Tr[1:P1Control.Trsize-1],P1Control.Pool$Tr[2:P1Control.Trsize]))
Trplot1 <- ggplot(P1Control.Trfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tr N") +
  ylab("Tr N+1") +
  ggtitle("P1 Baseline Tr") + 
  theme_bw()+ 
  xlim(0,1)+
  ylim(0,1)

#P21 Control TrBaseline
P21Control.Pool <- as.data.frame(P21Control.Pool)
P21Control.Trfigure <- as.data.frame(cbind(P21Control.Pool$Tr[1:P21Control.Trsize-1],P21Control.Pool$Tr[2:P21Control.Trsize]))
hull <- P21Control.Trfigure %>%
  slice(chull(V1,V2))
P21Control.Trfigure <- as.data.frame(cbind(P21Control.Pool$Tr[1:P21Control.Trsize-1],P21Control.Pool$Tr[2:P21Control.Trsize]))
Trplot2 <- ggplot(P21Control.Trfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tr N") +
  ylab("Tr N+1") +
  ggtitle("P21 Baseline Tr") + 
  theme_bw()+ 
  xlim(0,1)+
  ylim(0,1)


#P56 Control TrBaseline
P56Control.Pool <- as.data.frame(P56Control.Pool)
P56Control.Trfigure <- as.data.frame(cbind(P56Control.Pool$Tr[1:P56Control.Trsize-1],P56Control.Pool$Tr[2:P56Control.Trsize]))
hull <- P56Control.Trfigure %>%
  slice(chull(V1,V2))
P56Control.Trfigure <- as.data.frame(cbind(P56Control.Pool$Tr[1:P56Control.Trsize-1],P56Control.Pool$Tr[2:P56Control.Trsize]))
Trplot3 <- ggplot(P56Control.Trfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tr N") +
  ylab("Tr N+1") +
  ggtitle("P56 Baseline Tr") + 
  theme_bw()+ 
  xlim(0,1)+
  ylim(0,1)


#P21 Control TrHypercapnia
P21ControlHyper.Pool <- as.data.frame(P21ControlHyper.Pool)
P21ControlHyper.Trfigure <- as.data.frame(cbind(P21ControlHyper.Pool$Tr[1:P21ControlHyper.Trsize-1],P21ControlHyper.Pool$Tr[2:P21ControlHyper.Trsize]))
hull <- P21ControlHyper.Trfigure %>%
  slice(chull(V1,V2))
P21ControlHyper.Trfigure <- as.data.frame(cbind(P21ControlHyper.Pool$Tr[1:P21ControlHyper.Trsize-1],P21ControlHyper.Pool$Tr[2:P21ControlHyper.Trsize]))
Trplot4 <- ggplot(P21ControlHyper.Trfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tr N") +
  ylab("Tr N+1") +
  ggtitle("P21 Hypercapnia Tr") + 
  theme_bw()+ 
  xlim(0,1)+
  ylim(0,1)


#P56 Control TrHypercapnia
P56ControlHyper.Pool <- as.data.frame(P56ControlHyper.Pool)
P56ControlHyper.Trfigure <- as.data.frame(cbind(P56ControlHyper.Pool$Tr[1:P56ControlHyper.Trsize-1],P56ControlHyper.Pool$Tr[2:P56ControlHyper.Trsize]))
hull <- P56ControlHyper.Trfigure %>%
  slice(chull(V1,V2))
P56ControlHyper.Trfigure <- as.data.frame(cbind(P56ControlHyper.Pool$Tr[1:P56ControlHyper.Trsize-1],P56ControlHyper.Pool$Tr[2:P56ControlHyper.Trsize]))
Trplot5 <- ggplot(P56ControlHyper.Trfigure, aes(V1,V2)) +
  geom_point(shape=1, color="red") +
  geom_polygon(data=hull, color= "black", fill=NA) +
  stat_ellipse(level=0.68, geom= "polygon", alpha=0.5, fill = "gray") +
  xlab("Tr N") +
  ylab("Tr N+1") +
  ggtitle("P56 Hypercapnia Tr") + 
  theme_bw()+ 
  xlim(0,1)+
  ylim(0,1)



ggarrange(mean.Trplot, ellipse.Trplot, polygon.Trplot, Trplot1, Trplot2, Trplot3, blank, Trplot4, Trplot5, common.legend = TRUE, labels = c("A","B","C","D","E","F","","G","H"))




