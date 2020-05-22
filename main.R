chunk1 = read.csv2("student-mat.csv", sep = ',')
chunk2 = read.csv2("student-por.csv", sep = ',')

str(chunk1)
str(chunk2)
head(chunk1[1:3,])

df = merge(chunk1, chunk2,
           by=c("school","sex","age","address","famsize","Pstatus","Medu",
                "Fedu","Mjob","Fjob","reason","nursery","internet")
           )
print(nrow(df)) # 382 students (doubled students)

pairs(df[27:33])
