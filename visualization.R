#####################################################

# PLOTTING FALSE COLOUR IMAGES AND SIMCA RESULTS

#Author: Andrea Faltynkova
 #2023


#####################################################


 


plot(B)  #just plotting as a raster


#there are four layers in the raster, one for each polymer type. 

#inspecting the object will show which layers correspond to which polymer. Recall that each layer is binary (here -1 or 1). 

B

#see that layers 1,2,3,4 correspond to PE, PP, PET, PS respectively

#We can use dropLayer for plotting polymers individually: 
 
plot(dropLayer(B,c(2:4)))



#If you want to overlay the results onto an image, it's best to use the tiff files that were
#written to the output folder. 

# we can overlay the result onto a false colour image to visualize the particles and the results


setwd("~")

image<- brick("sample_image.tif")
plotRGB(image)  #false colour representation of the hyperspectral image


setwd("~/output")

PE<-raster("sample_sample_image.dat_Comp.PE.tiff")      #import the files
PP<-raster("sample_sample_image.dat_Comp.PP.tiff")
PET<-raster("sample_sample_image.dat_Comp.PET.tiff")
PS<-raster("sample_sample_image.dat_Comp.PS.tiff")

#extent(image)    #ensure the extent of the background image matches the extent of the imported rasters
#extent(PET)

extent(PE) <- c(0,251,0,270)     #--> set extent of each raster
extent(PET) <- c(0,251,0,270)
extent(PS) <- c(0,251,0,270)
extent(PP) <- c(0,251,0,270)



pal<- colorRampPalette(c( "deeppink", "green", "blue", "orange"))  #choose a colour palette

plotRGB(flip(image,vertical=T),1,2,3,axes=T)  #plot base image (small bug, need to flip the image vertically)
plot(PE,  add=T,col=pal(1), alpha= 0.7)   #overlay particles identified as PE
plot(PP, add=T, col= pal(2), alpha=0.7)   #overlay particles identified as PP
plot(PS, add=T, col=pal(3), alpha= 0.7)   #no PS particles
plot(PET, add=T, col=pal(4), alpha= 0.7)  #no PET particles


