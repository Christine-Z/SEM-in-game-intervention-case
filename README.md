# Game Intervention Case

## Dataset
experiential learning(expl): el1, el2, el3, el4                    
learning motivation(lemo): m1, m2                  
self-efficacy(seff): se1, se2                      
academic performance(acpe): ap1, ap2, ap3, ap4           
Gamification: Yes/No                          
119 out of 206 students receiving the serious game intervention

## Library

dplyr, stringr, Matrix, lavaan(Modeling), semPlot(Visualization)

## DIY Functions
Matrixtrace (Maximum Likelihood estimation)                                     
Fml(Parameter Estimation)

## Model Specification
### Model 1: four-factor Single-group SEM
**Model 1**
```
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
```
**Model 2**
```
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
```
> Globel Fit
```
fitMeasures(fit,c("chisq", "df", "pvalue", "cfi", "tli","srmr","rmsea"), output = "matrix")
```
| indice  | Model 1 | Model 2 | Threshold |
| ------- | ------- | ------- | --------- |
| Chisq  | 126.707  | 88.449  | NA  |
| df  | 48  | 46  | NA  |
| p-value  | 0  | 0  | >0.05  |
| CFI  | 0.914  | 0.954  | >0.95  |
| TLI  | 0.882  | 0.934  | >0.95  |
| SRMR  | 0.055  | 0.051  | <0.08  |
| RMSEA  | 0.089  | 0.067  | >0.07  |

# Multi-group SEM
Basic Model: Model 2
Group by: Gamification
Configural Invariance: no constraint between groups
Metric Invariance: constrain the same loadings between groups
Scalar Invariance: constrain the same loadings and intercepts between groups

```
anova(fit_mg_1, fit_mg_2,fit_mg_3)
```
|     | Configural | Metric | Scalar |
| ------- | ------- | ------- | --------- |
| df  | 92  | 100  | 108  |
| AIC  | 5967.5  | 5959.7  | 5955.7  |
| Chisq  | 159.11  | 167.29  | 179.27  |
| p-value  | NA  | 0.42  | 0.15  |

Metric Invariance and Scalar Invariance are not significantly better fit to the data compared to Configural Invariance (p = 0.42 and p = 0.15)
