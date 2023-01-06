##################################################

# HYPERSPECTRAL IMAGE ANALYSIS FOR DETECTING MICROPLASTICS

# Author: Andrea Faltynkova
# 2023 


##################################################





#INSTALL AND LOAD LIBRARIES
#--------------------------------------------------------------------------




#install.packages(c("ggfortify", "tidyr", "plyr", "dplyr", "devtools", "ggsci","mdatools","EEM","spectacles","ids","rgl","tiff","grid","terra","rgdal",
             #      "rgeos","remotes", "signal", "ggpubr", "hyperSpec","rasterVis", "Rcpp", "raster", "tidyverse"))



library(ggfortify)
library(tidyr)
library(plyr)
library(dplyr)
library(devtools)
library(ggsci)
library(mdatools)
library(EEM)
library(readxl)
library(spectacles)
library(ids)
library(rgl)
library(tiff)
library(grid)
library(terra)
library(rgdal)
library(rgeos)
library(remotes)
library(signal)
library(ggpubr)
library(hyperSpec)
library(rasterVis)
library(Rcpp)
library(raster)
library(tidyverse)





# SPECTRAL PREPROCESSING 
#--------------------------------------------------------------------------

setwd("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline")

source("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline/functions.R") #source functions from script


load(file= "data/raw_spectra.rda")    #import raw spectral data (with IDs)
load(file= "data/white_standard.rda") #import white reflectance standard
load(file= "data/wavelength.rda")     #import wavelengths  

spectra<- raw_spectra


Spectra_norm <- normalization_by_lightsource_(spectra, avg.standard) #normalize all spectra using the white reflectance standard


random_ids<- random_id(n= nrow(Spectra_norm), bytes= 6)  #add unique ID to each spectrum
spectra_unique <- cbind.data.frame(random_ids,Spectra_norm)


spectra_df <- SpectraDataFrame(wl= as.vector(Wavelength[-c(1:11)]),             #convert dataframe to a spectral object
                               nir= as.matrix(spectra_unique[,-c(1:12)]),       #Removing first 10 wavelengths due to noise
                               id= as.character(spectra_unique$random_ids),
                               data=spectra_unique)

k<- kernel("daniell", 7) #make kernel, window size 7      

spectra_preprocessed<- apply_spectra(spectra_df, kernapply, k)     #preprocess using smoothing kernel  

spectra_preprocessed <-spectra_preprocessed %>% 
  apply_spectra(diff,1) %>%                            #1st derivative
  apply_spectra(snv)                                  # Standard normal variate  

spectral_database <- cbind(spectra[,c(1:2)],    #Extract NIR spectra from spectral object
                           as.data.frame(spectra_preprocessed@nir),
                           stringsAsFactors=F)

rownames(spectral_database)<- 1:nrow(spectral_database)       #Add back IDs and colnames                    
spectral_database<- cbind.data.frame(c(1:nrow(spectral_database)),spectral_database)
Xnames<- paste0("X", colnames(spectral_database[,-c(1:3)]))
colnames(spectral_database) <- c("list", "posID", "origin", Xnames)


#spectral_database is now used to build the SIMCA model







# SIMCA MODEL 
#--------------------------------------------------------------------------



Test_SIMCA <- slice_sample(spectral_database, n=(0.2*nrow(spectral_database)))  #take random 20% of database for test set
colnames(Test_SIMCA)<- colnames(spectral_database)
exclude_SIMCA <- as.numeric(Test_SIMCA[,1])

Train_SIMCA<- spectral_database[-exclude_SIMCA,]  #remainder is used for training 
colnames(Train_SIMCA)<- colnames(spectral_database)
rownames(Train_SIMCA)<- make.names(Train_SIMCA$list, unique=TRUE)

TrainX_PE<- Train_SIMCA %>%         #divide training set by class (polymer type)
  dplyr::filter(posID == "PE")
TrainX_PE <- TrainX_PE[,-(1:3)]

TrainX_PET<-Train_SIMCA %>%
  dplyr::filter(posID == "PET")
TrainX_PET <- TrainX_PET[,-(1:3)]

TrainX_PP<-Train_SIMCA %>%
  dplyr::filter(posID == "PP")
TrainX_PP <- TrainX_PP[,-(1:3)]

TrainX_PS<-Train_SIMCA %>%
  dplyr::filter(posID == "PS")
TrainX_PS <- TrainX_PS[,-(1:3)]

TestX_SIMCA <- data.frame(Test_SIMCA[,-c(1:3)])              #separate test set by X (spectra) and Y (polymer id)
rownames(TestX_SIMCA) <- make.names(Test_SIMCA$list, unique= TRUE)
TestY_SIMCA <- Test_SIMCA$posID

SIMCA_PE <- simca(      #Create PCA for training set, one for each class 
  TrainX_PE,
  "PE",
  ncomp = min(nrow(TrainX_PE) - 1, ncol(TrainX_PE) - 1,5),
  x.test = TestX_SIMCA,
  c.test = TestY_SIMCA,
  cv = list("rand",5,5)
)

SIMCA_PET <- simca(
  TrainX_PET,
  "PET",
  ncomp = min(nrow(TrainX_PET) - 1, ncol(TrainX_PET) - 1, 4),
  x.test = TestX_SIMCA,
  c.test = TestY_SIMCA,
  cv = list("rand",5,5)
)

SIMCA_PS <- simca(
  TrainX_PS,
  "PS",
  ncomp = min(nrow(TrainX_PS) - 1, ncol(TrainX_PS) - 1, 5),
  x.test = TestX_SIMCA,
  c.test = TestY_SIMCA,
  cv = list("rand",5,5)
)

SIMCA_PP <- simca(
  TrainX_PP,
  "PP",
  ncomp = min(nrow(TrainX_PP) - 1, ncol(TrainX_PP) - 1, 3),
  x.test = TestX_SIMCA,
  c.test = TestY_SIMCA,
  cv = list("rand",5,5)
)

poly_model = simcam(list(SIMCA_PE, 
                         SIMCA_PP,
                         SIMCA_PET,
                         SIMCA_PS), 
                    info = "simcam model for polymer data") #combine PCAs into SIMCA for classification





# ANALYSIS PIPELINE
#--------------------------------------------------------------------------




#Set the working directory to a directory containing two folders: "input" which contains images to be analyzed
# and "output" where the results of the analysis will be stored



temp.dat<- list.files(path= "input", pattern="*.dat")       #Create list of file names in folder called "input" 
temp.hdr<-list.files(path= "input", pattern = "*.hdr")      # list header files separately 

setwd("~/input") #change wd to input folder

myfiles<- mapply(read.ENVI.Nicolet,   #import files 
                 file= temp.dat, 
                 headerfile= temp.hdr,
                 nicolet.correction=F)



##If importing hyspex files, use:

#temp.hyspex<- list.files(path= "input", pattern="*.hyspex")
#temp.hdr<-list.files(path= "input", pattern = "*.hdr")

##import files 
#setwd("/input")
#myfiles<- mapply(read.ENVI.HySpex, file= temp.dat, headerfile= temp.hyspex)



##be sure to change temp.dat below to temp.hyspex to preserve labeling from original file names




setwd("~/output") #change wd to output folder

for(i in 1:length(myfiles)) {
  
  HSI_images<-convert_envi(myfiles[[i]])   #convert HSI images to format accepted by model
  
  B<-polymer_classifyer(poly_model,HSI_images, Wavelength,avg.standard)    #classify spectra in image
  
  Q<- writeRaster(B, sprintf("sample_%s.tiff",       #preserve labeling from original file list
                  temp.dat[i]), 
                  format = "GTiff",         #write results as a .tiff file
                  overwrite= T, 
                  bylayer=T,    #write one .tiff for each layer in the raster stack (one image per polymer type)
                  suffix="names",
                  datatype="INT1U")
  
  
}

