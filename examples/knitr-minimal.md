This is a miminal example of using **knitr** with in HTML pages. I am actually using markdown here since it is more convenient in GitHub.

First, the input file was named as `knitr-minimal_knit_.md` ([click to view its source](https://github.com/mnel/R_code/blob/master/examples/knitr-minimal_knit_.md)), which contains `_knit_` so that **knitr** will automatically determine the output filename to be `knitr-minimal.md`. 

Note we may need to set up a few options before knitting this file, e.g. I used the code below to make sure **knitr** uses the correct hooks to wrap my output, and writes correct URL's for my images.

```
opts_knit$set(out.format = "gfm", base.url = "https://github.com/mnel/tree/master/R_code/examples/")
```



Now we write some code chunks in this markdown file:

```
## a simple calculator
1 + 1
```

```
## [1] 2
```

```
## boring random numbers
set.seed(123)
rnorm(5)
```

```
## [1] -0.56048 -0.23018  1.55871  0.07051  0.12929
```






We can also produce plots:

```
library(ggplot2)
qplot(hp, mpg, data = mtcars) + geom_smooth()
```

![plot of chunk md-cars-scatter](https://github.com/mnel/R_code/tree/master/examples/md-cars-scatter.png)


So no more hesitation on using GitHub and **knitr**! You just write a minimal amount of code to get beautiful output on the web.
