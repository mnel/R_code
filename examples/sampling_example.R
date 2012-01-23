
test_data <- as.data.frame(matrix(rnorm(1000), ncol=10, nrow = 100))
names(test_data) <- letters[1:10]
test_data$ID <- paste(sample(letters[1:26],10),sample(100,10),sep='_')

library(plyr)
test_data[,1:10] <- apply(test_data[,1:10],2, function(.x){.na <- sample(1:100, floor(runif(1,3,30))); .x[.na] <- NA; .x})

test_data$.row_index <- 1:100

test_data_melt <- reshape2:::melt(test_data, id = c('ID','.row_index'), na.rm = T)
training_rate <- function(value, rate, id.col,...){
  .other_args <- list(...)
  sample_ids <- .other_args[[id.col]]
  
  n_sample <- floor(0.7* length(value))
  
  sampled <- sample.int(length(value), size = n_sample)
  
  data_training <- value[sampled]
  data_validation <- value[-sampled]
  data_all <- value
  
  id_training <- sample_ids[sampled]
  id_validation <- sample_ids[-sampled]
  llist(data_training, data_validation, data_all, id_training, id_validation, sample_ids)
}


dlply(test_data_melt,.(variable), splat(training_rate), rate =0.7, id.col='.row_index')