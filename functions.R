

normalization_by_lightsource_<- function(INPUT_spectra,lightsource){
  
  INPUT_spectra[,-c(1:2)] %>%
    t() %>%
    "/" (lightsource) %>%
    t() %>%
    
    as.data.frame()
  
  
}



polymer_classifyer<-function(model,pixels, wave,lightsource) {
  
  pixelsA<-pixels[,-c(1,2)] %>%
    t() %>%
    "/" (lightsource) %>%
    t() %>%
    as.data.frame()
  
  randos<- ids::random_id(n=nrow(pixels),bytes=10, use_openssl = T)
  
  pixelsB<-cbind.data.frame(randos,pixelsA)
  
  colnames(pixelsB) <- c("ID",wave)
  
  spectra_pixels <- SpectraDataFrame(wl= wave[-c(1:8)], nir= as.matrix(pixelsB[-c(1:9)]),id= as.character(pixelsB[,1]), data= pixelsB)
  
  k<- kernel("daniell", 7)
  
  process_pixel<- apply_spectra(spectra_pixels,kernapply, k)
  
  process_pixel<-process_pixel %>% 
    apply_spectra(diff, 1) %>%
    apply_spectra(snv) 
  
  
  
  put_into_model <- as.data.frame(process_pixel@nir[,-c(1:3)])
  
  
  res = predict(model, put_into_model)
  
  SIMCA_res<-as.data.frame(res$c.pred)
  
  SIMCA_res_xy<- cbind.data.frame(pixels[,c(1,2)], SIMCA_res,stringsAsFactors=T)
  
  g<- rasterFromXYZ(SIMCA_res_xy)
  
  return(g)
  
}


convert_envi <- function(hyp_obj){
  
  df_specy<- as.wide.df(hyp_obj)
  df_specy<-df_specy[,-259]
  df_specy<- as.data.frame(df_specy)
  return(df_specy)
  
}
