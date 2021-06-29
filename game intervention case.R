#######################
Environment preparation
#######################

### DATA MANIPULATION ###
library("dplyr")     
library('stringr')
library('Matrix')#rankMatrix

### MODELING ###
library("lavaan")

### VISUALIZATION ###
library("semPlot")#semPaths

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

############
DIY Function
############

### Maximum Likelihood estimation ###
Matrixtrace <- function(A) {
  n <- dim(A)[1] # get dimension of matrix
  tr <- 0 # initialize trace value
  # Loop over the diagonal elements of the supplied matrix and add the element to tr
  for (k in 1:n) {
    l <- A[k,k]
    tr <- tr + l
  }
  return(tr[[1]])
}
### Parameter Estimation ###
Fml <- function(fit,data_cov){
  S <- as.matrix(data_cov)
  Sigma <- abs(inspect(fit, "cov.ov") %>% as.matrix())
  inter_matrix <- abs(S %*% solve(Sigma))
  Fml <- round(log(abs(S))-log(abs(Sigma))+Matrixtrace(inter_matrix)-rankMatrix(S)[1],2)
  return(Fml)
}
###########################
Data Import and Exploration
###########################
fdata <- read.csv('Data.csv',header = T)
cov_data <- fdata[,!names(fdata) == 'Gamification']

fdata_cov <- cov(cov_data)
fdata_cor <- round(cov2cor(fdata_cov),2)

###################
Model Specification
###################

### Model 1: four-factor Single-group SEM ###
model_1 <- " 
#experiential learning
expl =~ el1+ el2+ el3+ el4

#learning motivation
lemo =~ m1+ m2

#self-efficacy
seff =~ se1+ se2

# academic performance
acpe =~ ap1+ ap2+ ap3+ ap4
"
fit_1 <- cfa(model_1, data = fdata)
fit_1_sum <- summary(fit_1,standardized=T, fit.measure=T)

### Parameter Estimation ###
Fml(fit_1,fdata_cov)

####################
Model Evaluation
####################
gf_1 <- fitMeasures(fit_1,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")#global fit
mi_1 <- modificationIndices(fit_1,sort. =TRUE,maximum.number=10)# Local Fit

######################
Model Re-specification
######################

### Model 2: four-factor Single-group SEM ###
model_2 <- " 
#experiential learning
expl =~ el1+ el2+ el3+ el4

#learning motivation
lemo =~ m1+ m2

#self-efficacy
seff =~ se1+ se2

# academic performance
acpe =~ ap1+ ap2+ ap3+ ap4

#residual covariance
el3 ~~ se1
ap1 ~~ ap2
"
fit_2 <- sem(model_2, data = fdata)
fit_2_sum <- summary(fit_2,standardized=T, fit.measure=T)

### Model Evaluation ###
gf_2 <- fitMeasures(fit_2,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")#global fit
modificationIndices(fit_2,sort. =TRUE,maximum.number=10)# Local Fit

### Parameter Estimation ###
Fml(fit_2,fdata_cov)

#########
Plotting
#########
semPaths(fit_2, "est", nCharNodes = 0)

##################
Multi-group SEM
##################

### Configural invariance ###
fit_mg_1 <- sem(model_2, data = fdata, group = "Gamification")
### Metric Invariance ###
fit_mg_2 <- sem(model_2, data = fdata, group = "Gamification",
                group.equal = c("loadings"))
### Scalar Invariance ###
fit_mg_3 <- sem(model_2, data = fdata, group = "Gamification",
                group.equal = c("loadings","intercepts"))

anova(fit_mg_1, fit_mg_2,fit_mg_3)

gf_mg_1 <- fitMeasures(fit_mg_1,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
gf_mg_2 <- fitMeasures(fit_mg_2,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
gf_mg_3 <- fitMeasures(fit_mg_3,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
gf_mg <- round(cbind(gf_mg_1,gf_mg_2,gf_mg_3),3)
#Metric Invariance and Scalar Invariance are not significantly better fit to the data compared to Configural Invariance

fit_mg_1_sum <- summary(fit_mg_1,standardized=T, fit.measure=T)