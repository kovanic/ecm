source("import.r")

library("qgraph")
 #добавление пространственной переменной
 #строю матрицу смежности 
 neighbors <- read_xlsx("neighbors.xlsx")
 S <- as.matrix(neighbors[,2:ncol(neighbors)])
 a <- neighbors[,1]
 rownames(S) <- a[[1]]
 
 qgraph(S)
 
 for(i in 1:(nrow(S)-1)){
   for(j in (i+1):ncol(S)){
     S[j,i] <- S[i,j]
   }
 }
 
 # У нас для каждого года есть вектор легализации (медицинской -- M и рекреационной -- R),
 # поэтому количество соседей, легализовавших M составляет SM, а легализовавших R -- SR.
 # Важно, что и в векторе и в матрице штаты должны идти в алфавитном порядке.
 
 med_neighbors <- as.data.frame(cbind(tolower(a[[1]]), matrix(0,nrow = 51, ncol = length(c(1998:2019)))))
 names(med_neighbors) <- c("state", 1998:2019)
 
 rec_neighbors <- med_neighbors
 
 for(i in 2:ncol(med_neighbors)){
   base <- legal%>%
     mutate(med = tidyr::replace_na(med, 0),full = tidyr::replace_na(full, 0))%>%
     filter(year == as.integer(names(med_neighbors)[i]))%>%arrange(state)
   M <- as.matrix(base$med)
   R <- as.matrix(base$full)
   med_neighbors[,i] <-  S%*%M
   rec_neighbors[,i] <-  S%*%R
 }
 
 
 med_1 <- gather(med_neighbors, year, med_neighbors,names(med_neighbors)[2:ncol(med_neighbors)])
 rec_1 <- gather(rec_neighbors, year, rec_neighbors,names(rec_neighbors)[2:ncol(rec_neighbors)])
 
 med_1$year <- as.numeric(med_1$year)
 rec_1$year <- as.numeric(rec_1$year)
