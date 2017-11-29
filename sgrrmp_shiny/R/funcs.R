#' Get biological expectation with changing threshold and likelihoods
#' 
#' @param datin sf object with stream COMIDS and quantile expectations
#' @param thrsh numeric for CSCI scoring thresholds
#' @param likes numeric for tails to truncate expectations for overlap with thrsh
#' @param lbs chr string labels for interval classifications
#' 
#' @return a nested data frame sorted by increaesing median value of expected score of COMID and nested columns as original data, cut data by likes, and sream classification (strcls).  The strcls column indicates if the ranges in datcut are within, above, or below those defind by thrsh.
getcls <- function(datin, thrsh = 0.79, likes = 0.05, lbs = list('likely constrained' = 2, 'undetermined' = 1, 'likely unconstrained' = 0)){
  
  # sanity check
  if(likes >= 0.5)
    stop('likes must be less than 0.5')
  
  dat <- datin
  st_geometry(dat) <- NULL
  dat <- dat %>% 
    select(matches('^COMID$|^full0')) %>% 
    gather('var', 'val', -COMID) %>% 
    arrange(COMID, var) %>% 
    group_by(COMID) %>% 
    nest %>% 
    mutate(
      datcut = map(data, function(x){
        
        # get quantile labels to filter
        lovl <- 100 * likes
        hivl <- 100 * (1 -  likes) 
        vls <- c(lovl, hivl) %>% 
          str_pad(2, pad = '0') %>% 
          paste0('full0_', .)
        
        # filter by quantile labels and median
        x <- x %>% 
          filter(var %in% vls)
        
        return(x)
        
        
      }),
      
      medv = map(data, ~ filter(.x, var %in% 'full0_50') %>% .$val), 
      strcls = map(datcut, function(x){
        
        # return NA if any zero values in predictions
        if(any(x$val == 0)){
          
          cls <- NA
          return(cls)
          
        } 
        
        # get threshold range
        rngs <- x$val %>% 
          range
   
        # find if above/below or covering thrsh
        cls <- findInterval(thrsh, rngs)

        return(cls)
        
      })
      
    ) %>% 
    unnest(medv) %>% 
    arrange(medv) %>% 
    mutate(COMID = factor(COMID, levels = COMID)) %>% 
    unnest(strcls) 

  # subset lbs by those in interval
  lbs <- unique(dat$strcls) %>% 
    na.omit %>% 
    as.numeric %>% 
    match(unlist(lbs)) %>% 
    lbs[.]
  
  # strcls as correct factor levels
  dat <- dat %>%
    mutate(strcls = factor(strcls, levels = unlist(lbs), labels = names(lbs)))

  return(dat)
  
}

#' Get biological expectation with changing threshold and likelihoods, same as getcls but only returns class designation
#' 
#' @param datin sf object with stream COMIDS and quantile expectations
#' @param thrsh numeric for CSCI scoring thresholds
#' @param likes numeric for tails to truncate expectations for overlap with thrsh
#' @param lbs chr string labels for interval classifications
#' 
#' @return a nested data frame sorted by increaesing median value of expected score of COMID and nested columns as original data, cut data by likes, and sream classification (strcls).  The strcls column indicates if the ranges in datcut are within, above, or below those defind by thrsh.
getcls2 <- function(datin, thrsh = 0.79, likes = 0.05, lbs = list('likely constrained' = 2, 'undetermined' = 1, 'likely unconstrained' = 0)){
  
  # sanity check
  if(likes >= 0.5)
    stop('likes must be less than 0.5')
  
  dat <- datin
  st_geometry(dat) <- NULL
  dat <- dat %>% 
    select(matches('^COMID$|^full0')) %>% 
    gather('var', 'val', -COMID) %>% 
    arrange(COMID, var) %>% 
    group_by(COMID) %>% 
    nest %>% 
    mutate(
      strcls = map(data, function(x){
        
        # return NA if any zero values in predictions
        if(any(x$val == 0)){
          
          cls <- NA
          return(cls)
          
        } 
        
        # get quantile labels to filter
        lovl <- 100 * likes
        hivl <- 100 * (1 -  likes) 
        vls <- c(lovl, hivl) %>% 
          str_pad(2, pad = '0') %>% 
          paste0('full0_', .)
        
        # filter by quantile labels and median
        rngs <- x %>% 
          filter(var %in% vls) %>% 
          .$val %>% 
          range
        
        cls <- findInterval(thrsh, rngs)
     
        return(cls)

      })
     
    ) %>% 
    select(-data) %>% 
    unnest 
  
  # subset lbs by those in interval
  lbs <- unique(dat$strcls) %>% 
    na.omit %>% 
    as.numeric %>% 
    match(unlist(lbs)) %>% 
    lbs[.]
  
  # strcls as correct factor levels
  dat <- dat %>%
    mutate(strcls = factor(strcls, levels = unlist(lbs), labels = names(lbs)))
  
  return(dat)
  
}