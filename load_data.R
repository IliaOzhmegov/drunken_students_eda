# import some libraries --------------------------------------------------------
library(tidyverse)
library(dplyr)
library(magrittr)
library(plyr)

# Some useful functions --------------------------------------------------------
get_df <- function(path){
  if (!is.numeric(path)){
    read.csv2(path, sep = ',') %>% 
    mutate_at(vars(c("Dalc", "Walc", "famrel", "freetime", "goout", "health")), 
              ~mapvalues(.x %>% as.factor(), from=1:5, 
                         to=c("Very Low", "Low", "Medium", "High", "Very High"))) %>% 
    mutate_at(vars(ends_with("edu")),
              ~mapvalues(.x %>% as.factor(), from=0:4,
                         to=c("none", "primary education (4th grade)", 
                              "5th to 9th grade", "secondary education", 
                              "higher education"))
              )
  }else{
    stop("Path variable must be a string!")
  }
}

get_math_df <- function(path=NULL){
  df <- get_df("data/student-mat.csv")
}

get_port_df <- function(){
  df <- get_df("data/student-por.csv")
}

get_join_df <- function(){
  chunk1 <- get_math_df()
  chunk2 <- get_port_df()
  
  join_df = merge(chunk1, chunk2,
                  by=c("school",	    "sex",	      "age",	      "address",	
                       "famsize",    	"Pstatus",  	"Medu",	      "Fedu",	
                       "Mjob",	      "Fjob",	      "reason",	    "guardian",	
                       "traveltime",	"studytime",	"failures",	  "schoolsup",	
                       "famsup",	    "paid",	      "activities",	"nursery",	
                       "higher",	    "internet",	  "romantic",	  "famrel",	
                       "freetime",	  "goout",	    "Dalc",	      "Walc",	
                       "health",	    "absences"),
                  suffixes = c(".math",".port")
                 )
}

# loading all data -------------------------------------------------------------

math_df <- get_math_df()
port_df <- get_port_df()
join_df <- get_join_df()
full_df <- rbind(math_df, port_df)

# removing functions -----------------------------------------------------------
rm(get_df)
rm(get_math_df)
rm(get_port_df)
rm(get_join_df)








