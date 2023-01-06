source("C:/Users/andrfa/OneDrive - NTNU/Andrea F/IR-HSI-Microplastics-analysis-pipeline/2_functions.R")


Test_SIMCA <- slice_sample(spectral_database, n=(0.2*nrow(spectral_database)))
colnames(Test_SIMCA)<- colnames(spectral_database)
exclude_SIMCA <- as.numeric(Test_SIMCA[,1])

Train_SIMCA<- spectral_database[-exclude_SIMCA,]
colnames(Train_SIMCA)<- colnames(spectral_database)
rownames(Train_SIMCA)<- make.names(Train_SIMCA$list, unique=TRUE)


TrainX_PE<- Train_SIMCA %>%
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

TestX_SIMCA <- data.frame(Test_SIMCA[,-c(1:3)])
rownames(TestX_SIMCA) <- make.names(Test_SIMCA$list, unique= TRUE)
TestY_SIMCA <- Test_SIMCA$posID


SIMCA_PE <- simca(
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


poly_model = simcam(list(SIMCA_PE, SIMCA_PP, SIMCA_PET,SIMCA_PS), info = "simcam model for polymer data")
