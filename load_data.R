# Some useful functions --------------------------------------------------------
get_df <- function(path){
  if (!is.numeric(path)){
    df = read.csv2(path, sep = ',')
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

get_full_df <- function(){
  chunk1 <- get_math_df()
  chunk2 <- get_port_df()
  
  full_df = merge(chunk1, chunk2,
                  by=c("school","sex","age","address","famsize","Pstatus","Medu",
                      "Fedu","Mjob","Fjob","reason","nursery","internet"),
                  suffixes = c(".math",".port")
                 )
}

# loading all data -------------------------------------------------------------

math_df <- get_math_df()
port_df <- get_port_df()
full_df <- get_full_df()
