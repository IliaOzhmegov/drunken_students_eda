
get_math_df <- function(){
  math_df = read.csv2("data/student-mat.csv", sep = ',')
}

get_port_df <- function(){
  port_df = read.csv2("data/student-por.csv", sep = ',')
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

math_df <- get_math_df()
port_df <- get_port_df()
full_df <- get_full_df()