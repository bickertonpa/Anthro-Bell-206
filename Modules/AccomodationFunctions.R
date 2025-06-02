ScaleToPopulation <- function(sample,population)
  # scales column values of one dataset(data) using column means and sd of another dataset (population)
{
  scaled_sample <- data.frame() #create the return df
  col_mean <- data.frame()
  col_sd <- data.frame()
  cols <- list()
  population <- population %>% as.data.frame()
  #select columns that intersect between data and population
  cols <- intersect(names(sample),names(population)) %>% sort()
  #sort columns in data and population
  sample <- sample[cols]
  
  #calculate mean and sd of population columns
  col_mean <- population[cols] %>% na.omit %>% colMeans()
  col_sd <- population[cols] %>% na.omit %>% sapply(sd,na.rm = TRUE)
  
  for (i in 1:ncol(sample))
  {
    for (j in 1:nrow(sample)) 
    {
      scaled_sample[j,i] <- (sample[j,i] - col_mean[i])/col_sd[i]
    }
  }
  names(scaled_sample) <- names(sample)
  row.names(scaled_sample) <- row.names(sample)
  scaled_sample <- scaled_sample[cols]
  return(scaled_sample)
}


ScaleToPC <- function(StdScores,Rotation)
{
  StdScores <- StdScores %>% as.matrix()
  Std_Scores_sorted <- StdScores[,order(colnames(StdScores))]
  Rotation_sorted <- Rotation[order(rownames(Rotation)),]
  scores <- Std_Scores_sorted %*% Rotation_sorted %>% as.data.frame()
  return(scores)
}

AccommodationEllipse <- function(accomodation_level)
{
  accommodation <- accomodation_level
  a <- sqrt(qchisq((as.double(accommodation) / 100), df = 2)) #per DRDC-T PCA notes document
  b <- a
  
  vec <- seq(0, 2 * pi, length.out = 300)
  x_points <- a * cos(vec)
  y_points <- b * sin(vec)
  ellipse_frame <- data.frame(x = x_points, y = y_points)
  return(ellipse_frame)
}

PercentileSample <- function(sample,population)
  # returns the percentile of sample records give population data
{
  pcent_sample <- data.frame() #create the return df
  col_mean <- data.frame()
  col_sd <- data.frame()
  cols <- list()
  population <- population %>% as.data.frame()
  #select columns that intersect between data and population
  cols <- intersect(names(sample),names(population)) %>% sort()
  #sort columns in data and population
  sample <- sample[cols]
  
  #calculate mean and sd of population columns
  col_mean <- population[cols] %>% na.omit %>% colMeans()
  col_sd <- population[cols] %>% na.omit %>% sapply(sd,na.rm = TRUE)
  
  for (i in 1:ncol(sample))
  {
    for (j in 1:nrow(sample)) 
    {
      pcent_sample[j,i] <- pnorm(sample[j,i],mean = col_mean[i], sd = col_sd[i])
    }
  }
  names(pcent_sample) <- names(sample)
  row.names(pcent_sample) <- row.names(sample)
  pcent_sample <- pcent_sample[cols]
  return(pcent_sample)
}


scale_sample_based_on_population <- function(sample_data, population_data) 
  # Function to calculate scaled values (z-scores) of sample data based on population data
  # and to return the percentiles of the sample values
  {
  # Ensure that both datasets have the same columns
  common_columns <- intersect(names(sample_data), names(population_data))
  if (length(common_columns) == 0) {
    stop("No matching columns between sample_data and population_data.")
  }
  
  # Calculate population mean and standard deviation for each common column
  foo <- sum(is.na(population_data))
  population_mean <- sapply(population_data[common_columns], mean, na.rm = TRUE)
  population_sd <- sapply(population_data[common_columns], sd, na.rm = TRUE)
  
  # Scale each common column in sample_data using population mean and sd
  scaled_sample <- as.data.frame(lapply(common_columns, function(col) {
    (sample_data[[col]] - population_mean[[col]]) / population_sd[[col]]
  }))
  colnames(scaled_sample) <- common_columns
  
  # Calculate percentiles for sample values based on population data
  percentiles <- as.data.frame(lapply(common_columns, function(col) {
    pnorm(sample_data[[col]], mean = population_mean[[col]], sd = population_sd[[col]])
  }))
  colnames(percentiles) <- common_columns
  
  # Return both scaled values and percentiles as a list
  return(list(scaled_sample = scaled_sample, sample_percentiles = percentiles))
}