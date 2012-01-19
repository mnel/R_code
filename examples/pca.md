

This is a very basic demonstration of using `R` to perform principal component analysis.
Load some libraries and some example data. We will be using the meuse data set, trying to map the elevation.

```r
library(gstat)
data(meuse)
head(meuse)
```
```
##        x      y cadmium copper lead zinc  elev     dist   om ffreq soil
## 1 181072 333611    11.7     85  299 1022 7.909 0.001358 13.6     1    1
## 2 181025 333558     8.6     81  277 1141 6.983 0.012224 14.0     1    1
## 3 181165 333537     6.5     68  199  640 7.800 0.103029 13.0     1    1
## 4 181298 333484     2.6     81  116  257 7.655 0.190094  8.0     1    2
## 5 181307 333330     2.8     48  117  269 7.480 0.277090  8.7     1    2
## 6 181390 333260     3.0     61  137  281 7.791 0.364067  7.8     1    2
##   lime landuse dist.m
## 1    1      Ah     50
## 2    1      Ah     30
## 3    1      Ah    150
## 4    0      Ga    270
## 5    0      Ah    380
## 6    0      Ga    470
```
```r
## look at the elevation data
library(ggplot2)
ggplot(meuse, aes(x = x, y = y, size = elev)) + 
    geom_point() + coord_equal() + scale_size("Elevation")
```
![plot of chunk md-data-explore](https://github.com/mnel/R_code/raw/masterexamples/md-data-explore.png)




Now, we don't have many predictors to work with, so we will use a 2nd-order polynomial trend on the coordinates, as well as the distance.  The columns we want are `x,y,elev,dist`.

We begin by normalizing the `x` and `y` data to avoid numerical problems. **This is important**. 

```r
meuse$norm_x <- with(meuse, (x - min(x))/diff(range(x)))
meuse$norm_y <- with(meuse, (y - min(y))/diff(range(y)))
```



Then we use the function `prcomp` in the `stats` package. The `stats` package is usually loaded with `R`, but we will load it just in case.

```r
library(stats)
## derive the princical components model
pr_model <- prcomp(~norm_x + norm_y + I(norm_x^2) + 
    I(norm_y^2) + I(norm_x * norm_y) + dist, data = meuse)
summary(pr_model)
```
```
## Importance of components:
##                          PC1   PC2    PC3     PC4     PC5     PC6
## Standard deviation     0.596 0.219 0.0960 0.05970 0.02774 0.00993
## Proportion of Variance 0.852 0.115 0.0221 0.00855 0.00185 0.00024
## Cumulative Proportion  0.852 0.967 0.9894 0.99792 0.99976 1.00000
```



The first two principal components account for >95% of the variation.

We can look at some plots

```r
plot(pr_model, main = "Results of PCA on meuse data set")
```
![plot of chunk unnamed-chunk-3](https://github.com/mnel/R_code/raw/masterexamples/unnamed-chunk-3.png)


```r
biplot(pr_model, main = "Results of PCA on meuse data set")
```
![plot of chunk unnamed-chunk-4](https://github.com/mnel/R_code/raw/masterexamples/unnamed-chunk-4.png)

Note that a number of the red axes are almost co-linear, suggesting that a number of the variables are correlated (eg x^2 and x.y)


Next we create a dataframe with the x and y coordinates and the principal components. These components can be obtained using `predict()` without a `newdata` argument

```r
## create the data.frame
meuse_pca <- data.frame(meuse[, c("x", "y", "elev")], 
    predict(pr_model))
## look at it
head(meuse_pca)
```
```
##        x      y  elev    PC1      PC2       PC3     PC4       PC5
## 1 181072 333611 7.909 -1.178  0.27329  0.023581 0.09727  0.062612
## 2 181025 333558 6.983 -1.125  0.26566  0.033216 0.08656  0.062737
## 3 181165 333537 7.800 -1.199  0.14795  0.008259 0.10655  0.038365
## 4 181298 333484 7.655 -1.256  0.02847 -0.026725 0.12112  0.007142
## 5 181307 333330 7.480 -1.191 -0.07691 -0.034739 0.10861 -0.014073
## 6 181390 333260 7.791 -1.211 -0.18675 -0.053019 0.11641 -0.038013
##         PC6
## 1 -0.016420
## 2 -0.016521
## 3 -0.010903
## 4 -0.006048
## 5 -0.003633
## 6 -0.004137
```



To fit a basic linear model linear model with all the components 

```r
## create the formula
## this is short-cut to avoid lots of typing!
lm_formula <- as.formula(paste("elev ~", paste("PC", 
    1:6, sep = "", collapse = "+")))
## check this is correct!
lm_formula
```
```
## elev ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6
```
```r
## fit the model
lm_pc_full <- lm(lm_formula, data = meuse_pca)
## summarize
summary(lm_pc_full)
```
```
## 
## Call:
## lm(formula = lm_formula, data = meuse_pca)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8253 -0.6398  0.0603  0.6146  1.8138 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.1654     0.0698  117.05  < 2e-16 ***
## PC1          -0.3187     0.1174   -2.71   0.0074 ** 
## PC2          -2.6128     0.3190   -8.19  1.1e-13 ***
## PC3           0.9980     0.7287    1.37   0.1729    
## PC4          -1.6821     1.1722   -1.43   0.1534    
## PC5           3.7854     2.5226    1.50   0.1356    
## PC6          -3.2457     7.0508   -0.46   0.6460    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.868 on 148 degrees of freedom
## Multiple R-squared: 0.353,	Adjusted R-squared: 0.327 
## F-statistic: 13.5 on 6 and 148 DF,  p-value: 3.69e-12 
## 
```



We can perform stepwise backwards elimination to choose the optimal number of components. The function `stepAIC` in `MASS` will do this.

```r
library(MASS)
step_model_lm <- stepAIC(lm_pc_full, trace = 0)
## set trace = 1 if you want to see what is happening
summary(step_model_lm)
```
```
## 
## Call:
## lm(formula = elev ~ PC1 + PC2 + PC4 + PC5, data = meuse_pca)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.7775 -0.6608  0.0523  0.6513  1.8653 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   8.1654     0.0698  117.02  < 2e-16 ***
## PC1          -0.3187     0.1175   -2.71   0.0075 ** 
## PC2          -2.6128     0.3191   -8.19  1.1e-13 ***
## PC4          -1.6821     1.1726   -1.43   0.1535    
## PC5           3.7854     2.5233    1.50   0.1357    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
## 
## Residual standard error: 0.869 on 150 degrees of freedom
## Multiple R-squared: 0.344,	Adjusted R-squared: 0.327 
## F-statistic: 19.7 on 4 and 150 DF,  p-value: 4.9e-13 
## 
```



This suggests that 4 components are required -- PC1, PC2, PC4, PC5, although PC4 and PC5 have p-values around 0.15. 

We now move to fitting the spatial model

```r
## convert to geodata
pca_geodata <- as.geodata(meuse_pca, coord.cols = 1:2, 
    data.col = 3, covar.col = 4:9)
## look at initial variogram
initial_variogram <- variog(pca_geodata, trend = ~PC1 + 
    PC2 + PC4 + PC5, uvec = 20)
```
```
## variog: computing omnidirectional variogram
```
```r
plot(initial_variogram, pch = 19)
```
![plot of chunk md-initial-variogram](https://github.com/mnel/R_code/raw/masterexamples/md-initial-variogram.png)


From this plot it looks as if an initial guess of phi = 300, nugget = 0.5, sigmasq = 1 could be reasonable

```r
reml_model <- likfit(pca_geodata, trend = ~PC1 + 
    PC2 + PC4 + PC5, lik.method = "REML", ini.cov.pars = c(1, 
    300), nugget = 0.5)
```
```
## ---------------------------------------------------------------
## likfit: likelihood maximisation using the function optim.
## likfit: Use control() to pass additional
##          arguments for the maximisation function.
##         For further details see documentation for optim.
## likfit: It is highly advisable to run this function several
##         times with different initial values for the parameters.
## likfit: WARNING: This step can be time demanding!
## ---------------------------------------------------------------
## likfit: end of numerical maximisation.
```
```r
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model$cov.pars
nugget <- reml_model$nugget
## get coefficients
coefficients <- reml_model$beta
## get standard errors
se_error <- sqrt(diag(reml_model$beta.var))
## get t values
t_value <- coefficients/se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value), df = (nrow(meuse_pca) - 
    6))
## make pretty
coef_mat <- cbind(coefficients, se_error, t_value, 
    t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", 
    "t value", "Pr(>|t|)")
rownames(coef_mat) <- c("(intercept)", paste("PC", 
    c(1, 2, 4, 5), sep = ""))
printCoefmat(coef_mat)
```
```
##             Estimate  Std.Err t value Pr(>|t|)    
## (intercept)  8.08308  0.28119   28.75  < 2e-16 ***
## PC1         -0.00407  0.39782   -0.01  0.99185    
## PC2         -2.41777  0.61946   -3.90  0.00014 ***
## PC4         -0.47977  2.67836   -0.18  0.85808    
## PC5          1.63677  4.38382    0.37  0.70941    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```
```r
## the variance components
paste("SigmaSq = ", round(cov_pars[1], 3), ", Nugget = ", 
    round(nugget, 3), ", Phi = ", round(cov_pars[2], 3))
```
```
## [1] "SigmaSq =  0.492 , Nugget =  0.485 , Phi =  570.275"
```


Clearly, haven taken the spatial correlation into account, we can remove PC1, PC4 or PC5 from the 4 predictor model. We will look at the variogram fit to make sure it is reasonable:

```r
## look at the variogram fit
reml_variogram <- variog(pca_geodata, data = apply(reml_model$model.components[, 
    2:3], 1, sum), uvec = 20, max.dist = 2000)
```
```
## variog: computing omnidirectional variogram
```
```r
plot(reml_variogram)
lines(reml_model)
```
![plot of chunk unnamed-chunk-9](https://github.com/mnel/R_code/raw/masterexamples/unnamed-chunk-9.png)


If we drop PC1

```r
reml_model_245 <- likfit(pca_geodata, trend = ~PC2 + 
    PC4 + PC5, lik.method = "REML", ini.cov.pars = c(1, 300), 
    nugget = 0.5)
```
```
## ---------------------------------------------------------------
## likfit: likelihood maximisation using the function optim.
## likfit: Use control() to pass additional
##          arguments for the maximisation function.
##         For further details see documentation for optim.
## likfit: It is highly advisable to run this function several
##         times with different initial values for the parameters.
## likfit: WARNING: This step can be time demanding!
## ---------------------------------------------------------------
## likfit: end of numerical maximisation.
```
```r
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model_245$cov.pars
nugget <- reml_model_245$nugget
## get coefficients
coefficients <- reml_model_245$beta
## get standard errors
se_error <- sqrt(diag(reml_model_245$beta.var))
## get t values
t_value <- coefficients/se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value), df = (nrow(meuse_pca) - 
    5))
## make pretty
coef_mat <- cbind(coefficients, se_error, t_value, 
    t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", 
    "t value", "Pr(>|t|)")
rownames(coef_mat) <- c("(intercept)", paste("PC", 
    c(2, 4, 5), sep = ""))
printCoefmat(coef_mat)
```
```
##             Estimate Std.Err t value Pr(>|t|)    
## (intercept)    8.097   0.221   36.62  < 2e-16 ***
## PC2           -2.451   0.567   -4.32  2.8e-05 ***
## PC4           -0.544   2.392   -0.23     0.82    
## PC5            1.771   4.130    0.43     0.67    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```
```r
paste("SigmaSq = ", round(cov_pars[1], 3), ", Nugget = ", 
    round(nugget, 3), ", Phi = ", round(cov_pars[2], 3))
```
```
## [1] "SigmaSq =  0.4 , Nugget =  0.483 , Phi =  437.791"
```


We can drop PC4

```r
reml_model_25 <- likfit(pca_geodata, trend = ~PC2 + 
    PC5, lik.method = "REML", ini.cov.pars = c(1, 300), nugget = 0.5)
```
```
## ---------------------------------------------------------------
## likfit: likelihood maximisation using the function optim.
## likfit: Use control() to pass additional
##          arguments for the maximisation function.
##         For further details see documentation for optim.
## likfit: It is highly advisable to run this function several
##         times with different initial values for the parameters.
## likfit: WARNING: This step can be time demanding!
## ---------------------------------------------------------------
## likfit: end of numerical maximisation.
```
```r
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model_25$cov.pars
nugget <- reml_model_25$nugget
## get coefficients
coefficients <- reml_model_25$beta
## get standard errors
se_error <- sqrt(diag(reml_model_25$beta.var))
## get t values
t_value <- coefficients/se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value), df = (nrow(meuse_pca) - 
    4))
## make pretty
coef_mat <- cbind(coefficients, se_error, t_value, 
    t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", 
    "t value", "Pr(>|t|)")
rownames(coef_mat) <- c("(intercept)", paste("PC", 
    c(2, 5), sep = ""))
printCoefmat(coef_mat)
```
```
##             Estimate Std.Err t value Pr(>|t|)    
## (intercept)    8.101   0.203   39.84  < 2e-16 ***
## PC2           -2.450   0.545   -4.50  1.4e-05 ***
## PC5            1.821   4.013    0.45     0.65    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```
```r
paste("SigmaSq = ", round(cov_pars[1], 3), ", Nugget = ", 
    round(nugget, 3), ", Phi = ", round(cov_pars[2], 3))
```
```
## [1] "SigmaSq =  0.368 , Nugget =  0.484 , Phi =  403.901"
```



and even PC5.

```r
reml_model_2 <- likfit(pca_geodata, trend = ~PC2, 
    lik.method = "REML", ini.cov.pars = c(1, 300), nugget = 0.5)
```
```
## ---------------------------------------------------------------
## likfit: likelihood maximisation using the function optim.
## likfit: Use control() to pass additional
##          arguments for the maximisation function.
##         For further details see documentation for optim.
## likfit: It is highly advisable to run this function several
##         times with different initial values for the parameters.
## likfit: WARNING: This step can be time demanding!
## ---------------------------------------------------------------
## likfit: end of numerical maximisation.
```
```r
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model_2$cov.pars
nugget <- reml_model_2$nugget
## get coefficients
coefficients <- reml_model_2$beta
## get standard errors
se_error <- sqrt(diag(reml_model_2$beta.var))
## get t values
t_value <- coefficients/se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value), df = (nrow(meuse_pca) - 
    3))
## make pretty
coef_mat <- cbind(coefficients, se_error, t_value, 
    t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", 
    "t value", "Pr(>|t|)")
rownames(coef_mat) <- c("(intercept)", paste("PC", 
    2, sep = ""))
printCoefmat(coef_mat)
```
```
##             Estimate Std.Err t value Pr(>|t|)    
## (intercept)    8.102   0.198   40.90  < 2e-16 ***
## PC2           -2.425   0.534   -4.54  1.1e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```
```r
paste("SigmaSq = ", round(cov_pars[1], 3), ", Nugget = ", 
    round(nugget, 3), ", Phi = ", round(cov_pars[2], 3))
```
```
## [1] "SigmaSq =  0.356 , Nugget =  0.484 , Phi =  395.688"
```



Looking at the variogram
Clearly, haven taken the spatial correlation into account, we can remove PC1, PC4 or PC5 from the 4 predictor model. We will look at the variogram fit to make sure it is reasonable:

```r
## look at the variogram fit
reml_variogram_2 <- variog(pca_geodata, data = apply(reml_model_2$model.components[, 
    2:3], 1, sum), uvec = 20, max.dist = 2000)
```
```
## variog: computing omnidirectional variogram
```
```r
plot(reml_variogram_2)
lines(reml_model_2)
```
![plot of chunk unnamed-chunk-13](https://github.com/mnel/R_code/raw/masterexamples/unnamed-chunk-13.png)


To make the predictions we need the prediction grid, with the appropriately scaled x and y coordinates

```r
## load the data
data(meuse.grid)
## normalize the x and y -- note we use the minima and
#   difference in range from the sample data!
meuse.grid$norm_x <- (meuse.grid$x - min(meuse$x))/diff(range(meuse$x))
meuse.grid$norm_y <- (meuse.grid$y - min(meuse$y))/diff(range(meuse$y))
## the following is required because predict doesn't
#   parse formulae properly
meuse.grid[["I(norm_x^2)"]] <- meuse.grid$norm_x^2
meuse.grid[["I(norm_y^2)"]] <- meuse.grid$norm_y^2
meuse.grid[["I(norm_x * norm_y)"]] <- meuse.grid$norm_x + 
    meuse.grid$norm_y
## we then make the prediction of the principal
#   components
pca_meuse_grid <- data.frame(meuse.grid[, c("x", 
    "y")], predict(pr_model, newdata = meuse.grid))
```



We can then make a map. `gstat` is much quicker for prediction so we will use it!

```r
## convert to spatial objects
coordinates(meuse_pca) <- ~x + y
## the grid is a grid!
gridded(pca_meuse_grid) <- ~x + y
## convert the geoR model to gstat
reml_model_2_gstat <- as.vgm.variomodel(reml_model_2)
## krige (e-blup)
elevation_eblup <- krige(elev ~ PC2, meuse_pca, 
    pca_meuse_grid, model = reml_model_2_gstat)
```
```
## [using universal kriging]
```



We can look at the results.

```r
## to plot nicely use the raster package
## the e-blup
plot(raster(elevation_eblup, layer = 1), main = "E-BLUP of elevation")
```
![plot of chunk unnamed-chunk-16](https://github.com/mnel/R_code/raw/masterexamples/unnamed-chunk-161.png)```r
## and prediction error variance
plot(raster(elevation_eblup, layer = 2), main = "E-BLUP error variance")
```
![plot of chunk unnamed-chunk-16](https://github.com/mnel/R_code/raw/masterexamples/unnamed-chunk-162.png)


The raster package also makes it very easy to save in .IMG format that can be opened by ARC-GIS

```r
## save the e-blup
library(rgdal)
writeRaster(raster(elevation_eblup, layer = 1), 
    filename = "elev_eblup.IMG", format = "HFA")
## and prediction error variance
writeRaster(raster(elevation_eblup, layer = 2), 
    filename = "elev_pev.IMG", format = "HFA")
```





