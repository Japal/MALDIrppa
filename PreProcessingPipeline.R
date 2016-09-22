## Example robust MALDI-TOF mass spectra pre-processing using MALDIquant + MALDIrppa

# Load example raw data (in R format)
# Note: this collection of mass spectra is a small random sample from a real database, it has only been
# included for illustrative purposes.

data(spectra) # list of MassSpectra class objects (see MALDIquant package)
data(type)    # vector with metadata (spectra IDs)

# Required libraries ------------------------------------------------------

library(wmtsa)
library(MALDIquant)
library(MALDIrppa)
library(signal)
library(robustbase)

# Parameter settings

thScale <- 2.5
ite <- 105
SigNoi <- 3.5
hws <- 20
tol <- 0.003

# Initial screening (?screenSpectra for details) ----

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

# Trim
spectra <- trim(spectra,range=c(2500,13000))

# Peak detection ----

peaks <- detectPeaks(spectra,SNR=SigNoi,halfWindowSize=hws)

c <- countPeaks(peaks)
lab <- 1:length(c)
plot(c,col="white")
text(c,labels=lab)

# Peak alignment and binning ----

peaks <- alignPeaks(peaks,minFreq=0.9,tolerance=tol)

# Save pre-processed data ----

save(peaks,file="peaks.new.Rdata")
save(type,file="type.new.Rdata")

#################################################################################

## Post-processing

# Outlier detection at isolate level based on intensities ----

aty <- detectOutliers(peaks,by=type.new$Isolate)
peaks.clean <- peaks[aty[,2]==FALSE]
type.clean <- type[aty[,2]==FALSE,]

# Or, outlier detection at isolate level based on peak presence/absence patterns ----

aty <- detectOutliers(peaks,type$Isolate,binary=TRUE)
peaks.clean <- peaks[aty[,2]==FALSE]
type.clean <- type[aty[,2]==FALSE,]

# Filtering and merging ----

# Filtering out peaks occurring in less than 25% replicates within isolate (bio rep level)

peaks.clean.f <- filterPeaks(peaks.clean,minFreq=0.25,labels=type.clean$Isolate) # 344 peaks in intensity matrix

# Merge reps by the median

peaks.clean.fm <- mergeMassPeaks(peaks.clean.f,labels=type.clean$Isolate,method="median")

# Merge metadata accordingly

type.clean.fm <- aggregate(type.clean,list(Serotype=type.clean$Serotype,Biotype=type.clean$Biotype,Isolate=type.clean$Isolate),FUN=length)[,1:3]

# Obtain final intensity matrix ----

int.binary <- intensityMatrix(peaks.clean.fm)

# Storing resulting intensity matrix ----

write.table(type.clean.fm,file="type.clean.fm.csv",sep=",",row.names=FALSE)
writeIntensity(int,file="int.binary.clean.fm",format="csv",labels=type.clean.fm$Isolate)
?writeInt(peaks16.merged,file="intMatrix16.merged",format="nexus",binary=TRUE,labels=type16.merged)












