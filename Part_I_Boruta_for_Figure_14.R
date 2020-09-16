#Boruta
#-----
#Control Baseline
set.seed(111)
ControlBaseline.fboruta <- Boruta(Age~., data=ControlBaseline.fframe, doTrace=2, maxRuns=500)
print(ControlBaseline.fboruta)
plot(ControlBaseline.fboruta, las=2, cex.axis=0.5, main = "f Baseline Boruta", xlab="")

ControlBaseline.TVboruta <- Boruta(Age~., data=ControlBaseline.TVframe, doTrace=2, maxRuns=500)
print(ControlBaseline.TVboruta)
plot(ControlBaseline.TVboruta, las=2, cex.axis=0.5,  main = "TV Baseline Boruta", xlab="")

ControlHyper.fboruta <- Boruta(Age~., data=ControlHyper.fframe, doTrace=2, maxRuns=500)
print(ControlHyper.fboruta)
plot(ControlHyper.fboruta, las=2, cex.axis=0.5,  main = "f Hypercapnia Boruta", xlab="")

ControlHyper.TVboruta <- Boruta(Age~., data=ControlHyper.TVframe, doTrace=2, maxRuns=500)
print(ControlHyper.TVboruta)
plot(ControlHyper.TVboruta, las=2, cex.axis=0.5,  main = "TV Hypercapnia Boruta", xlab="")

par(mfrow=c(2,2))
plot(ControlBaseline.fboruta, las=2, cex.axis=0.5, main = "f Baseline Boruta", xlab="")
plot(ControlBaseline.TVboruta, las=2, cex.axis=0.5,  main = "TV Baseline Boruta", xlab="")
plot(ControlHyper.fboruta, las=2, cex.axis=0.5,  main = "f Hypercapnia Boruta", xlab="")
plot(ControlHyper.TVboruta, las=2, cex.axis=0.5,  main = "TV Hypercapnia Boruta", xlab="")