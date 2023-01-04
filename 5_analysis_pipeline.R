

#Set the working directory to a directory containing two folders: "input" which contains images to be analyzed
# and "output" where the results of the analysis will be stored



setwd("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline")

#Create list of file names in folder called "input" 
temp.dat<- list.files(path= "input", pattern="*.dat")
temp.hdr<-list.files(path= "input", pattern = "*.hdr")


#import files 
setwd("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline/input")
myfiles<- mapply(read.ENVI.Nicolet, file= temp.dat, headerfile= temp.hdr, nicolet.correction=F)



##If importing hyspex files, use:

#temp.hyspex<- list.files(path= "input", pattern="*.hyspex")
#temp.hdr<-list.files(path= "input", pattern = "*.hdr")

##import files 
#setwd("/input")
#myfiles<- mapply(read.ENVI.HySpex, file= temp.dat, headerfile= temp.hyspex)



##be sure to change temp.dat below to temp.hyspex to preserve labeling from original file names


setwd("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline/output")

for(i in 1:length(myfiles)) {
  
  HSI_images<-convert_envi(myfiles[[i]])
  
  B<-polymer_classifyer(poly_model,HSI_images, Wavelength,avg.standard)
  
  Q<- writeRaster(B, sprintf("sample_%s.tiff",temp.dat[i]), format = "GTiff", overwrite= T, bylayer=T, suffix="names",datatype="INT1U")
  
  
}

