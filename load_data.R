# import some libraries --------------------------------------------------------
library(tidyverse)

# Some useful functions --------------------------------------------------------
get_df <- function(path, subject_name){
  if (!is.numeric(path)){
    grades <- c("A", "B", "C", "D", "F")
    
    read.csv2(path, sep = ',') %>% 
    
    # we want to be polite
    rename(gender='sex') %>%
      
    # to get only one variable instead of two
    mutate(Salc = Dalc + Walc) %>% 
      
    # Changing variables to a more meaningful values
    mutate_at(vars(Salc), 
             ~plyr::mapvalues(.x %>% as.factor(), from=2:10, 
             to=c("Extremely Low", "Very Low", "Low", "Medium Low", "Medium", 
                  "Medium High", "High", "Very High", "Extremely High"))) %>% 
      
    # Changing variables to a more meaningful values
    mutate_at(vars(c("Dalc", "Walc", "famrel", "freetime", "goout", "health")), 
              ~plyr::mapvalues(.x %>% as.factor(), from=1:5, 
                         to=c("Very Low", "Low", "Medium", "High", "Very High"))) %>% 
      
    # Changing variables to a more meaningful values
    mutate_at(vars(ends_with("edu")),
              ~plyr::mapvalues(.x %>% as.factor(), from=0:4,
                         to=c("none", "primary education (4th grade)", 
                              "5th to 9th grade", "secondary education", 
                              "higher education"))
              ) %>% 
      
    # Adding another variable that correspond to name of the subject
    mutate(subject=subject_name %>% as.factor()) %>% 
     
    # Changing variables to a more meaningful values
      mutate(G3_d = case_when(
        G3 >= 16 ~ grades[1],
        G3 >= 14 & G3 < 16 ~ grades[2],
        G3 >= 12 & G3 < 14 ~ grades[3],
        G3 >= 10 & G3 < 12 ~ grades[4],
        G3 < 10 ~ grades[5]
      ) %>% as.factor()
      ) 
    
  }else{
    stop("Path variable must be a string!")
  }
}

get_math_df <- function(path=NULL){
  df <- get_df("data/student-mat.csv", subject_name = "math")
}

get_port_df <- function(){
  df <- get_df("data/student-por.csv", subject_name = "port")
}

get_join_df <- function(){
  chunk1 <- get_math_df()
  chunk2 <- get_port_df()
  
  join_df = merge(chunk1, chunk2,
                  by=c("school",     "gender",	  "age",        "address",	
                       "famsize",    "Pstatus",   "Medu",       "Fedu",	
                       "Mjob",       "Fjob",      "reason",     "guardian",	
                       "traveltime", "studytime", "failures",   "schoolsup",	
                       "famsup",     "paid",      "activities",	"nursery",	
                       "higher",     "internet",  "romantic",   "famrel",	
                       "freetime",   "goout",     "Dalc",       "Walc",	
                       "health",     "absences"),
                  suffixes = c(".math",".port")
                 )
}

# loading all data -------------------------------------------------------------

math_df <- get_math_df()
port_df <- get_port_df()
full_df <- rbind(math_df, port_df)

# removing functions -----------------------------------------------------------
rm(get_df)
rm(get_math_df)
rm(get_port_df)
rm(get_join_df)

rm(math_df)
rm(port_df)

# saving CSV and Rds files -----------------------------------------------------

write_csv(full_df, "data/student-full.csv")
write_rds(full_df, "data/student-full.rds")

# loading Matlab color scheme --------------------------------------------------
matlab.colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

colours_5 = RColorBrewer::brewer.pal(5, 'RdYlGn')[5:1]
colours_9 = RColorBrewer::brewer.pal(9, 'RdYlGn')[9:1]

