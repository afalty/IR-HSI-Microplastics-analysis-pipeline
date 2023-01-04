# making a mulitcoloured plot 


#just plotting as a raster
plot(B)


#there are four layer in the raster, one for each polymer type. 

#inspecting the object will show which layers correspond to which polymer. Recall that each layer is binary (here -1 or 1). 

B

#see that layers 1,2,3,4 correspond to PE, PP, PET, PS respectively

#We can use dropLayer for plotting polymers individually: 
PE<- dropLayer(B,c(2:4))
plot(PE)





#If you want to overlay the results onto an image, it's best to use the tiff files that were
#written to the output folder so that the extents of the various images match. 

# we can overlay the result onto a .tif file of the image to visualize the particles and the results


setwd("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline")

image<- brick("grid.tif")
plotRGB(image)



setwd("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline/output")


#import the files


PE<-raster("sample_grid.dat_Comp.PE.tiff")
PP<-raster("sample_grid.dat_Comp.PP.tiff")
PET<-raster("sample_grid.dat_Comp.PET.tiff")
PS<-raster("sample_grid.dat_Comp.PS.tiff")


#ensure the extent of the background image matches the extent of the imported rasters:
#extent(image)

#0,20, 0, 20 --> set extent of each raster
extent(PE) <- c(0,20,0,20)
extent(PET) <- c(0,20,0,20)
extent(PS) <- c(0,20,0,20)
extent(PP) <- c(0,20,0,20)


#choose a colour palette

pal<- colorRampPalette(c( "deeppink", "green", "blue", "orange"))

#plot 

plotRGB(image,1,2,3,axes=T)
plot(PE,  add=T,col=pal(1), alpha= 0.7)
plot(PP, add=T, col= pal(2), alpha=0.7)
plot(PS, add=T, col=pal(3), alpha= 0.7)
plot(PET, add=T, col=pal(4), alpha= 0.7)




#this isn't very exciting just looking at a grid, so you can check the folder "sample plotting" for more demonstrative examples. 

# this small grid was used as an example as larger images contain too much data to be hosted on Github. This is a constructed image
# made just for demonstration purposes. 

