## Example robust MALDI-TOF mass spectra pre-processing using MALDIquant + MALDIrppa

# Last revised on 10/10/16

# Load example raw data (in R format)

data(spectra) # list of MassSpectra class objects
data(type)    # metadata

# Load package and set pre-processing parameters ----

library(MALDIrppa)

# Parameter settings

thScale <- 2.5
ite <- 105
SigNoi <- 3.5
hws <- 20
tol <- 0.003

# Initial screening ----

sc.results <- screenSpectra(spectra,type)
summary(sc.results)
plot(sc.results)

spectra <- sc.results$fspectra
type <- sc.results$fmeta

# Denoising, baseline correction, normalisation ----

spectra <- transformIntensity(spectra, method="sqrt")
spectra <- wavSmoothing(spectra,thresh.scale=thScale)
spectra <- removeBaseline(spectra,method="SNIP",iterations=ite)
spectra <- calibrateIntensity(spectra,method="PQN")

# Trim range ----

# spectra <- trim(spectra,range=c(2500,13000))

# Peak detection ----

peaks <- detectPeaks(spectra,SNR=SigNoi,halfWindowSize=hws)

# Peak alignment and binning ----

peaks <- alignPeaks(peaks,minFreq=0.9,tolerance=tol)

# Save pre-processed data ----

save(peaks,file="peaks.new.Rdata")
save(type,file="type.new.Rdata")

#################################################################################

## Post-processing

rm(list=ls())
load("peaks.new.Rdata")
load("type.new.Rdata")

# Outlier detection at isolate level based on intensities ----

aty <- detectOutliers(peaks,by=type$Isolate)
peaks.clean <- peaks[aty[,2]==FALSE] # discard outlying peaks
type.clean <- type[aty[,2]==FALSE,] # and corresponding metadata

# Or, outlier detection at isolate level based on peak presence/absence patterns ----

aty <- detectOutliers(peaks,type$Isolate,binary=TRUE)
peaks.clean <- peaks[aty[,2]==FALSE]
type.clean <- type[aty[,2]==FALSE,]

# Filtering and merging replicates ----

# Discard peaks occurring in less than 25% replicates within isolate (bio rep level)

peaks.clean.f <- filterPeaks(peaks.clean,minFreq=0.25,labels=type.clean$Isolate)

# Merge reps by the median

peaks.clean.fm <- mergeMassPeaks(peaks.clean.f,labels=type.clean$Isolate,method="median")

# Merge metadata accordingly

type.clean.fm <- aggregate(type.clean,list(Serotype=type.clean$Serotype,Biotype=type.clean$Biotype,Isolate=type.clean$Isolate),FUN=length)[,1:3]

# Obtain final intensity matrix ----

int.clean.fm <- intensityMatrix(peaks.clean.fm)

# Storing resulting intensity matrix ----

writeMetadata(type.clean.fm,filename="type.clean.fm",format="csv")
writeIntensity(int.clean.fm,filename="int.clean.fm",format="csv",labels=type.clean.fm$Isolate)
writeIntensity(int.clean.fm,filename="int.clean.fm",format="NEXUS",labels=type.clean.fm$Isolate)












