

#Set the working directory to a directory containing two folders: "input" which contains images to be analyzed
# and "output" where the results of the analysis will be stored



setwd("path_to_wd")

#Create list of file names in folder called "input" 
temp.dat<- list.files(path= "input", pattern="*.dat")
temp.hdr<-list.files(path= "input", pattern = "*.hdr")


#import files 
setwd("/input")
myfiles<- mapply(read.ENVI.Nicolet, file= temp.dat, headerfile= temp.hdr, nicolet.correction=F)



#If importing hyspex files, use:

#temp.hyspex<- list.files(path= "input", pattern="*.hyspex")
#temp.hdr<-list.files(path= "input", pattern = "*.hdr")

#import files 
#setwd("/input")
#myfiles<- mapply(read.ENVI.HySpex, file= temp.dat, headerfile= temp.hyspex)




setwd("~/output")

for(i in 1:length(myfiles)) {
  
  HSI_images<-convert_envi(myfiles[[i]])
  
  B<-polymer_classifyer(poly_model,HSI_images, wavelength,avg.standard)
  
  Q<- writeRaster(B, sprintf("sample_%s.tiff",temp.hyspex[i]), format = "GTiff", overwrite= T, bylayer=T, suffix="names",datatype="INT1U")
  
  
}

