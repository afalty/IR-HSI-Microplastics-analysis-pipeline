load(file= "data/raw_spectra.rda")
load(file= "data/white_standard.rda")
load(file= "data/wavelength.rda")

spectra<- raw_spectra



#normalize all spectra using the white reflectance standard
Spectra_norm <- normalization_by_lightsource_(spectra, avg.standard)

#add unique ID to each spectrum
random_ids<- random_id(n= nrow(Spectra_norm), bytes= 6)
spectra_unique <- cbind.data.frame(random_ids,Spectra_norm)

#convert dataframe to a spectral object. Removing first 10 wavelengths due to noise.
spectra_df <- SpectraDataFrame(wl= as.vector(Wavelength[-c(1:11)]), nir= as.matrix(spectra_unique[,-c(1:12)]), id= as.character(spectra_unique$random_ids), data=spectra_unique)

#preprocess using smoothing, 1st derivative and snv
k<- kernel("daniell", 7)

spectra_preprocessed<- apply_spectra(spectra_df, kernapply, k)

spectra_preprocessed <-spectra_preprocessed %>% 
  apply_spectra(diff,1) %>%
  apply_spectra(snv) 


#Extract NIR spectra from spectral object
spectral_database <- cbind(spectra[,c(1:2)],as.data.frame(spectra_preprocessed@nir),stringsAsFactors=F)


#Add back IDs and colnames
rownames(spectral_database)<- 1:nrow(spectral_database)
spectral_database<- cbind.data.frame(c(1:nrow(spectral_database)),spectral_database)
Xnames<- paste0("X", colnames(spectral_database[,-c(1:3)]))
colnames(spectral_database) <- c("list", "posID", "origin", Xnames)



#spectral_database is used to build the SIMCA model
