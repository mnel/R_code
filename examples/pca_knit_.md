
<!--roptions dev=png,fig.width=5,fig.height=5 -->
This is a very basic demonstration of using `R` to perform principal component analysis.
Load some libraries and some example data. We will be using the meuse data set, trying to map the elevation.

<!--begin.rcode md-data-explore
library(gstat)
data(meuse)
head(meuse)
## look at the elevation data
library(ggplot2)
ggplot(meuse,aes(x=x,y=y,size=elev)) + geom_point() + coord_equal() + scale_size('Elevation')
end.rcode-->



Now, we don't have many predictors to work with, so we will use a 2nd-order polynomial trend on the coordinates, as well as the distance.  The columns we want are `x,y,elev,dist`.

We begin by normalizing the `x` and `y` data to avoid numerical problems. **This is important**. 

<!--begin.rcode md-normalize-coordinates
meuse$norm_x <- with(meuse, (x- min(x)) / diff(range(x)))
meuse$norm_y <- with(meuse, (y- min(y)) / diff(range(y)))
end.rcode-->

Then we use the function `prcomp` in the `stats` package. The `stats` package is usually loaded with `R`, but we will load it just in case.

<!--begin.rcode md-run-pca
library(stats)
## derive the princical components model 
pr_model <- prcomp( ~ norm_x + norm_y + I(norm_x^2) + I(norm_y^2) + I(norm_x*norm_y) + dist, data =meuse)
summary(pr_model)
end.rcode-->

The first two principal components account for >95% of the variation.

We can look at some plots

<!--begin.rcode md-pca-plot 
 plot(pr_model, main = 'Results of PCA on meuse data set')
end.rcode-->

<!--begin.rcode md-pca-biplot 
 biplot(pr_model, main = 'Results of PCA on meuse data set')
end.rcode-->
Note that a number of the red axes are almost co-linear, suggesting that a number of the variables are correlated (eg x^2 and x.y)


Next we create a dataframe with the x and y coordinates and the principal components. These components can be obtained using `predict()` without a `newdata` argument

<!--begin.rcode md-predict-pca
## create the data.frame
meuse_pca <- data.frame(meuse[,c('x','y','elev')], predict(pr_model))
## look at it
head(meuse_pca)
end.rcode-->

To fit a basic linear model linear model with all the components 

<!--begin.rcode md-fit-lm
## create the formula
## this is short-cut to avoid lots of typing!
lm_formula <- as.formula(paste('elev ~', paste('PC',1:6, sep='', collapse='+')))
## check this is correct!
lm_formula
## fit the model
lm_pc_full <- lm(lm_formula, data = meuse_pca)
## summarize
summary(lm_pc_full)
end.rcode -->

We can perform stepwise backwards elimination to choose the optimal number of components. The function `stepAIC` in `MASS` will do this.

<!--begin.rcode md-stepwise
library(MASS)
step_model_lm <- stepAIC(lm_pc_full, trace = 0)
## set trace = 1 if you want to see what is happening
summary(step_model_lm)
end.rcode-->

This suggests that 4 components are required -- PC1, PC2, PC4, PC5, although PC4 and PC5 have p-values around 0.15. 

We now move to fitting the spatial model

<!--begin.rcode md-initial-variogram
## convert to geodata
pca_geodata <- as.geodata(meuse_pca, coord.cols= 1:2, data.col=3, covar.col=4:9)
## look at initial variogram
initial_variogram <- variog(pca_geodata, trend = ~ PC1 + PC2 + PC4 + PC5, uvec=20, messages = F)
plot(initial_variogram, pch = 19)
end.rcode-->

From this plot it looks as if an initial guess of phi = 300, nugget = 0.5, sigmasq = 1 could be reasonable

<!--begin.rcode md-fit-reml-spatial
reml_model <- likfit(pca_geodata, trend =  ~ PC1 + PC2 + PC4 + PC5, lik.method = 'REML', ini.cov.pars = c(1,300), nugget = 0.5, message = F )
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model$cov.pars
nugget <- reml_model$nugget
## get coefficients
coefficients <- reml_model$beta
## get standard errors
se_error <- sqrt(diag(reml_model$beta.var))
## get t values
t_value <- coefficients / se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value) ,df = (nrow(meuse_pca) - 6))
## make pretty
coef_mat <- cbind(coefficients,se_error,t_value,t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)") 
rownames(coef_mat) <- c('(intercept)',paste('PC',c(1,2,4,5),sep=''))
printCoefmat(coef_mat)
## the variance components
paste('SigmaSq = ', round(cov_pars[1],3), ', Nugget = ', round(nugget,3), ', Phi = ', round(cov_pars[2],3))
end.rcode-->
Clearly, haven taken the spatial correlation into account, we can remove PC1, PC4 or PC5 from the 4 predictor model. We will look at the variogram fit to make sure it is reasonable:

<!--begin.rcode md-reml-variogram
## look at the variogram fit
reml_variogram <- variog(pca_geodata, data = apply(reml_model$model.components[,2:3],1,sum),uvec = 20, max.dist = 2000, messages = F)
plot(reml_variogram)
lines(reml_model)
end.rcode-->

If we drop PC1

<!--begin.rcode md-reml-step-1
reml_model_245 <- likfit(pca_geodata, trend =  ~  PC2 + PC4 + PC5, lik.method = 'REML', ini.cov.pars = c(1,300), nugget = 0.5, message = F )
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model_245$cov.pars
nugget <- reml_model_245$nugget
## get coefficients
coefficients <- reml_model_245$beta
## get standard errors
se_error <- sqrt(diag(reml_model_245$beta.var))
## get t values
t_value <- coefficients / se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value) ,df = (nrow(meuse_pca) - 5))
## make pretty
coef_mat <- cbind(coefficients,se_error,t_value,t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)") 
rownames(coef_mat) <- c('(intercept)',paste('PC',c(2,4,5),sep=''))
printCoefmat(coef_mat)
paste('SigmaSq = ', round(cov_pars[1],3), ', Nugget = ', round(nugget,3), ', Phi = ', round(cov_pars[2],3))
end.rcode-->
We can drop PC4

<!--begin.rcode md-reml-step-2
reml_model_25 <- likfit(pca_geodata, trend =  ~  PC2 + PC5, lik.method = 'REML', ini.cov.pars = c(1,300), nugget = 0.5, message = F )
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model_25$cov.pars
nugget <- reml_model_25$nugget
## get coefficients
coefficients <- reml_model_25$beta
## get standard errors
se_error <- sqrt(diag(reml_model_25$beta.var))
## get t values
t_value <- coefficients / se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value) ,df = (nrow(meuse_pca) - 4))
## make pretty
coef_mat <- cbind(coefficients,se_error,t_value,t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)") 
rownames(coef_mat) <- c('(intercept)',paste('PC',c(2,5),sep=''))
printCoefmat(coef_mat)
paste('SigmaSq = ', round(cov_pars[1],3), ', Nugget = ', round(nugget,3), ', Phi = ', round(cov_pars[2],3))
end.rcode-->

and even PC5.

<!--begin.rcode md-reml-step-3
reml_model_2 <- likfit(pca_geodata, trend =  ~  PC2, lik.method = 'REML', ini.cov.pars = c(1,300), nugget = 0.5, messages = F)
## summarize (this is a bit ugly, but will work)
cov_pars <- reml_model_2$cov.pars
nugget <- reml_model_2$nugget
## get coefficients
coefficients <- reml_model_2$beta
## get standard errors
se_error <- sqrt(diag(reml_model_2$beta.var))
## get t values
t_value <- coefficients / se_error
## and probabilities
t_prob <- 2 * pt(-abs(t_value) ,df = (nrow(meuse_pca) - 3))
## make pretty
coef_mat <- cbind(coefficients,se_error,t_value,t_prob)
colnames(coef_mat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)") 
rownames(coef_mat) <- c('(intercept)',paste('PC',2,sep=''))
printCoefmat(coef_mat)
paste('SigmaSq = ', round(cov_pars[1],3), ', Nugget = ', round(nugget,3), ', Phi = ', round(cov_pars[2],3))
end.rcode-->

Looking at the variogram
Clearly, haven taken the spatial correlation into account, we can remove PC1, PC4 or PC5 from the 4 predictor model. We will look at the variogram fit to make sure it is reasonable:

<!--begin.rcode md-variogram-final
## look at the variogram fit
reml_variogram_2 <- variog(pca_geodata, data = apply(reml_model_2$model.components[,2:3],1,sum),uvec = 20, max.dist = 2000, messages = F)
plot(reml_variogram_2)
lines(reml_model_2)
end.rcode-->

To make the predictions we need the prediction grid, with the appropriately scaled x and y coordinates

<!--begin.rcode md-load-grid
## load the data
data(meuse.grid)
## normalize the x and y -- note we use the minima and difference in range from the sample data!
meuse.grid$norm_x <- (meuse.grid$x- min(meuse$x)) / diff(range(meuse$x))
meuse.grid$norm_y <- (meuse.grid$y- min(meuse$y)) / diff(range(meuse$y))
## the following is required because predict doesn't  parse formulae properly
meuse.grid[['I(norm_x^2)']] <- meuse.grid$norm_x^2
meuse.grid[['I(norm_y^2)']] <- meuse.grid$norm_y^2
meuse.grid[['I(norm_x * norm_y)']] <- meuse.grid$norm_x + meuse.grid$norm_y
## we then make the prediction of the principal components
pca_meuse_grid <- data.frame(meuse.grid[,c('x','y')], predict(pr_model, newdata = meuse.grid))
end.rcode-->

We can then make a map. `gstat` is much quicker for prediction so we will use it!

<!--begin.rcode md-predict
## convert to spatial objects
coordinates(meuse_pca) <- ~ x+y
## the grid is a grid!
gridded(pca_meuse_grid) <- ~ x+y
## convert the geoR model to gstat
reml_model_2_gstat <- as.vgm.variomodel(reml_model_2)
## krige (e-blup)
elevation_eblup <- krige(elev ~ PC2, meuse_pca, pca_meuse_grid, model =reml_model_2_gstat )
end.rcode-->

We can look at the results.

<!--begin.rcode md-eblup-plot
## to plot nicely use the raster package
library(raster)
## the e-blup
plot(raster(elevation_eblup, layer = 1), main = 'E-BLUP of elevation')
end.rcode-->
<!--begin.rcode md-pev-plot
## and prediction error variance
plot(raster(elevation_eblup, layer = 2), main = 'E-BLUP error variance')
end.rcode-->

The raster package also makes it very easy to save in .IMG format that can be opened by ARC-GIS

<!--begin.rcode eval = F
## save the e-blup
library(rgdal)
writeRaster(raster(elevation_eblup, layer = 1), filename = 'elev_eblup.IMG', format = 'HFA')
## and prediction error variance
writeRaster(raster(elevation_eblup, layer = 2), filename = 'elev_pev.IMG', format = 'HFA')
end.rcode-->


([click to view the R code](https://github.com/mnel/R_code/raw/master/examples/pca_knit_.R))


