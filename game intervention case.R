#######################
Environment preparation
#######################

### DATA MANIPULATION ###
library("haven")        
library("dplyr")     
library("psych")
library('stringr')
library('Matrix')

### MODELING ###
library("lavaan")
library("semPlot")

### VISUALIZATION ###
library("tidySEM")
library("purrr")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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

#Model 1: four-factor Single-group SEM
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

####################
parameter Estimation
####################

#Maximum Likelihood estimation
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

S <- as.matrix(fdata_cov)
Sigma <- abs(inspect(fit_1, "cov.ov") %>% as.matrix())
inter_matrix <- abs(S %*% solve(Sigma))
Fml <- log(abs(S))-log(abs(Sigma))+Matrixtrace(inter_matrix)-rankMatrix(S)[1]
Fml_1 <- round(Fml,2)
write.csv(Fml_1,"D:/Download/KUL/2nd semester/Structural Equation/project/output/Fml_1.csv")

####################
Model Evaluation
####################

#global fit 
gf_1 <- fitMeasures(fit_1,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
write.csv(gf_1,"D:/Download/KUL/2nd semester/Structural Equation/project/output/gf_1.csv")
# Local Fit
mi_1 <- modificationIndices(fit_1,sort. =TRUE,maximum.number=10)
write.csv(mi_1,"D:/Download/KUL/2nd semester/Structural Equation/project/output/mi_1.csv")

######################
Model Re-specification
######################

#Model 2: four-factor Single-group SEM
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

#global fit
gf_2 <- fitMeasures(fit_2,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
write.csv(gf_2,"D:/Download/KUL/2nd semester/Structural Equation/project/output/gf_2.csv")

# Local Fit
modificationIndices(fit_2,sort. =TRUE,maximum.number=10)

#parameter estimation

S <- as.matrix(fdata_cov)
Sigma <- inspect(fit_2, "cov.ov") %>% as.matrix()
inter_matrix <- abs(S %*% solve(Sigma))
Fml <- log(abs(S))-log(abs(Sigma))+Matrixtrace(inter_matrix)-rankMatrix(S)[1]
Fml_2 <- round(Fml,2)
write.csv(Fml_2,"D:/Download/KUL/2nd semester/Structural Equation/project/output/Fml_1.csv")

#########
Plotting
#########

semPaths(fit_2, "est", nCharNodes = 0)

##################
Multi-group SEM
##################

#Configural invariance
fit_mg_1 <- sem(model_2, data = fdata, group = "Gamification")
#Metric Invariance
fit_mg_2 <- sem(model_2, data = fdata, group = "Gamification",
                group.equal = c("loadings"))
#Scalar Invariance
fit_mg_3 <- sem(model_2, data = fdata, group = "Gamification",
                group.equal = c("loadings","intercepts"))

anova(fit_mg_1, fit_mg_2,fit_mg_3)

gf_mg_1 <- fitMeasures(fit_mg_1,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
gf_mg_2 <- fitMeasures(fit_mg_2,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
gf_mg_3 <- fitMeasures(fit_mg_3,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")

gf_mg <- round(cbind(gf_mg_1,gf_mg_2,gf_mg_3),3)

fit_mg_1_sum <- summary(fit_mg_1,standardized=T, fit.measure=T)


##################
Test Model
##################

tm <- " 
#experiential learning
expl =~ el1+ el2+ el3+ el4

#learning motivation
lemo =~ m1+ m2+ c*seff

#self-efficacy(mediator models)
seff =~ a1*se1+ a2*se2

# academic performance
acpe =~ ap1+ ap2+ ap3+ ap4

#residual covariance
el3 ~~ se1
ap1 ~~ ap2

#indirect effects (IDE)
a1c3 := a1*c
a2c3 := a2*c
# total effect
total := a1*c+ a2*c
"
fit_t <- sem(tm, data = fdata)

summary(fit_t,standardized=T, fit.measure=T)

fitMeasures(fit_t,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
# Local Fit
modificationIndices(fit_t,sort. =TRUE,maximum.number=10)


S <- as.matrix(fdata_cov)
Sigma <- inspect(fit_t, "cov.ov") %>% as.matrix()
inter_matrix <- abs(S %*% solve(Sigma))
Fml <- log(abs(S))-log(abs(Sigma))+Matrixtrace(inter_matrix)-rankMatrix(S)[1]
Fml_t <- round(Fml,2)
