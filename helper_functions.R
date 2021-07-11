# this is the function for Humidity Correction (Crilley et al. 2020) --> (nach Lorenz Harr)
humidity_correction_above <- function(data, K = 0.33) {
  round(data$PM25/ (1+((K/1.65)/(-1+(1/((data$RH)/100))))), digits = 1)
}

humidity_correction_below <- function(data) {
  round(data$PM25, digits = 1)
}

easy_humCorrectPM_tres <- function(data, PMColumn, RelHumColumn, K=0.33, HumTreshold=60)
{
  data[,ncol(data)+1] <- NA
  colnames(data)[ncol(data)] <- "PM2.5_HumCor"
  ja <- nrow(data)
  for (i in 1:ja) {
    
    measuredHum <- data[i,RelHumColumn]
    
    if(measuredHum > HumTreshold )
    {
      data[i,ncol(data)] <- round((data[i,PMColumn]/ (1+((K/1.65)/(-1+(1/((measuredHum)/100)))))), digits = 1)
    }
    else{
      data[i,ncol(data)] <- round(data[i,PMColumn], digits = 1)
      
    }
  }
  return (data)
}


# lookup function for corrected data --> (nach Lorenz Harr)
get_value_180 <- function(device_number, variable) {
  
  filtered_df <- cali_180 %>% 
    filter(device == device_number & var == variable)
  
  return_value <- filtered_df$value
  
  return(return_value)
  
}

get_value_20 <- function(device_number, variable) {
  
  filtered_df <- cali_20 %>% 
    filter(device == device_number & var == variable)
  
  return_value <- filtered_df$value
  
  return(return_value)
  
}


adjust_PM_180sec <- function(data, PMColumn, MODELNUMBER)
{
  
  if(MODELNUMBER == "NR1")
  {
    data[, ncol(data)+1] <- 3.729e-05  * (data[,PMColumn])^4 -2.190e-03 * (data[,PMColumn])^3  + 4.015e-02 * (data[,PMColumn])^2 + 8.893e-01 * data[,PMColumn] + 1.307e-01 
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR1"
    
  } else if(MODELNUMBER == "NR3"){
    
    data[, ncol(data)+1] <- -2.363e-05 * (data[,PMColumn])^4 + 1.937e-03 * (data[,PMColumn])^3  -5.179e-02 * (data[,PMColumn])^2 + 1.219e+00  * data[,PMColumn] -2.339e-01
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR3"
  } else if(MODELNUMBER == "NR4"){
    
    data[, ncol(data)+1] <- -2.028e-05 * (data[,PMColumn])^4 + 2.361e-04 * (data[,PMColumn])^3 + 2.289e-02 * (data[,PMColumn])^2 + 1.024e+00  * data[,PMColumn] + 1.722e-01 
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR4"
  } else if(MODELNUMBER == "NR6"){
    
    data[, ncol(data)+1] <- -2.213e-05  * (data[,PMColumn])^4 + 7.406e-04 * (data[,PMColumn])^3 + 5.796e-03 * (data[,PMColumn])^2 + 9.638e-01 * data[,PMColumn] +1.528e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR6"
  } else if(MODELNUMBER == "NR7"){
    
    data[, ncol(data)+1] <- data[,PMColumn]
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR7"
  } else if(MODELNUMBER == "NR2"){
    
    data[, ncol(data)+1] <- -8.722e-06 * (data[,PMColumn])^4 + 3.985e-04 * (data[,PMColumn])^3 -5.222e-03 * (data[,PMColumn])^2 + 1.112e+00  * data[,PMColumn] -1.383e-01
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR2"
  } else if(MODELNUMBER == "NR8"){
    
    data[, ncol(data)+1] <- -9.155e-06 * (data[,PMColumn])^4 + 8.836e-04 * (data[,PMColumn])^3 -2.818e-02 * (data[,PMColumn])^2 + 1.030e+00 * data[,PMColumn]  -2.818e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.NR8"
  }
  
  
  return(data)
}


adjust_PM_180sec_DEZ_Measures <- function(data, PMColumn, MODELNUMBER)
{
  
  if(MODELNUMBER == "NR1")
  {
    data[, ncol(data)+1] <- 2.744e-05  * (data[,PMColumn])^4 -1.146e-03 * (data[,PMColumn])^3  + 1.265e-02 * (data[,PMColumn])^2 + 1.047e+00 * data[,PMColumn] +  3.514e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR1"
    
  } else if(MODELNUMBER == "NR3"){
    
    data[, ncol(data)+1] <- -1.543e-05 * (data[,PMColumn])^4 +  1.376e-03 * (data[,PMColumn])^3  -3.896e-02 * (data[,PMColumn])^2 + 1.138e+00  * data[,PMColumn] -3.220e-01
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR3"
  } else if(MODELNUMBER == "NR4"){
    
    data[, ncol(data)+1] <-  4.587e-06* (data[,PMColumn])^4 -3.203e-04 * (data[,PMColumn])^3 + 6.734e-03 * (data[,PMColumn])^2 + 1.150e+00  * data[,PMColumn] +  4.262e-02 
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR4"
  } else if(MODELNUMBER == "NR6"){
    
    data[,ncol(data)+1] <- 0.5576535 * data[,PMColumn] + 0.2122843
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR6"
  } else if(MODELNUMBER == "NR7"){
    
    data[, ncol(data)+1] <- data[,PMColumn]
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR7"
  } else if(MODELNUMBER == "NR2"){
    
    data[, ncol(data)+1] <- 6.181e-06  * (data[,PMColumn])^4 -2.488e-04  * (data[,PMColumn])^3 + 1.676e-03 * (data[,PMColumn])^2 + 1.071e+00  * data[,PMColumn]  -2.527e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR2"
  } else if(MODELNUMBER == "NR8"){
    
    data[, ncol(data)+1] <- -1.873e-05 *(data[,PMColumn])^4 + 1.376e-03*(data[,PMColumn])^3 + -3.294e-02 * (data[,PMColumn])^2 +  9.492e-01 * (data[,PMColumn]) -4.853e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR8"
  }
  
  
  return(data)
}

adjust_To_Mean_PM_180Sec_Early21 <- function(data, PMColumn, MODELNUMBER)
{
  
  if(MODELNUMBER == "NR1")
  {
    data[, ncol(data)+1] <- 2.744e-05  * (data[,PMColumn])^4 -1.146e-03 * (data[,PMColumn])^3  + 1.265e-02 * (data[,PMColumn])^2 + 1.047e+00 * data[,PMColumn] +  3.514e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR1"
    
  } else if(MODELNUMBER == "NR3"){
    
    data[, ncol(data)+1] <- -1.543e-05 * (data[,PMColumn])^4 +  1.376e-03 * (data[,PMColumn])^3  -3.896e-02 * (data[,PMColumn])^2 + 1.138e+00  * data[,PMColumn] -3.220e-01
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR3"
  } else if(MODELNUMBER == "NR4"){
    
    data[, ncol(data)+1] <-  4.587e-06* (data[,PMColumn])^4 -3.203e-04 * (data[,PMColumn])^3 + 6.734e-03 * (data[,PMColumn])^2 + 1.150e+00  * data[,PMColumn] +  4.262e-02 
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR4"
  } else if(MODELNUMBER == "NR6"){
    
    data[,ncol(data)+1] <- 1.110e-06 *(data[,PMColumn])^4 -2.027e-04 *(data[,PMColumn])^3 + 6.484e-03 * (data[,PMColumn])^2 +  9.523e-01 * (data[,PMColumn]) +   2.996e-02
    
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR6"
  } else if(MODELNUMBER == "NR7"){
    
    data[, ncol(data)+1] <- data[,PMColumn]
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR7"
  } else if(MODELNUMBER == "NR2"){
    
    data[, ncol(data)+1] <- 6.181e-06  * (data[,PMColumn])^4 -2.488e-04  * (data[,PMColumn])^3 + 1.676e-03 * (data[,PMColumn])^2 + 1.071e+00  * data[,PMColumn]  -2.527e-02
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR2"
  } else if(MODELNUMBER == "NR8"){
    data[, ncol(data)+1] <- -1.873e-05 *(data[,PMColumn])^4 + 1.376e-03*(data[,PMColumn])^3 + -3.294e-02 * (data[,PMColumn])^2 +  9.492e-01 * (data[,PMColumn]) -4.853e-02
    
    colnames(data) [ncol(data)] <- "ADJ_180sec.PM2.5.NR8"
  }
  
  
  return(data)
}


