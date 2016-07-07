## Example robust MALDI-TOF data pre-processing using MALDIquant + robMALDIppa

# Load raw data (in R format)

load("spectra.ori.Rdata") # raw MS profiles
load("type.ori.Rdata") # associated metadata

# Required libraries ------------------------------------------------------

suppressPackageStartupMessages(library(wmtsa))
suppressPackageStartupMessages(library(MALDIquant))
suppressPackageStartupMessages(library(MALDIrppa))
suppressPackageStartupMessages(library(signal))
suppressPackageStartupMessages(library(robustbase))

# Parameter settings

thScale <- 2.5
ite <- 105
SigNoi <- 3.5
hws <- 20
tol <- 0.003

# Initial screening ----

sc.results <- screenSpectra(spectra,type)

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

## Post-processing (see also examples in robMALDIppa help system)

# Atypical samples detection at isolate level (based on actual intensities) ----

aty <- detectOutliers(peaks,by=type.new$Isolate)
peaks.clean <- peaks[aty[,2]==FALSE]
type.clean <- type[aty[,2]==FALSE,]

# Or, atypical samples detection at isolate level (based on peak presence/absence patterns) ----

aty <- detectOutliers(peaks,type$Isolate,binary=TRUE)
peaks.clean <- peaks[aty[,2]==FALSE]
type.clean <- type[aty[,2]==FALSE,]

# Filtering and merging ----

# Filter out peaks occurring in less than 25% replicates within isolate (bio rep level)

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












