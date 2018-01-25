###########################
#
# Data Import
#
###########################

library(readxl)    
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}


## main function for reading from xlsx file (extract different sheet)
r_excel_class <- function(filename,n){
  Data <- read_excel_allsheets(filename)
  
  if(is.numeric(n))
    D <- as.data.frame(Data[[n]]) 
  
  if(is.character(n))
    D <- as.data.frame(Data[[which(names(Data)==n)]])
  
  rownames(D) <- D[,1]
  D <- D[,-1]
  D
}


## Reading names from xlsx
r_excel_names <- function(filename){
  Data <- read_excel_allsheets(filename)
  Class_names <- names(Data)
  rownames <- unlist((Data[[1]])[,1] ,use.names = FALSE)
  date <- colnames(as.data.frame(Data[[1]]))[-1]
  out <- list(Class_names,rownames,date)
  names(out) <- c("class","student","date")
  return(out)
}


###########################
#
# Data Labeling for each class and course
#
###########################


## Inserting By hand
num_CP <- 3  # number of Class P
class_label <- c("M","P","C")
##


## Label names, dates for each calss
for(i in 2:num_CP){
  nam <- paste("class_P", i, sep = "")
  Data_num <- paste("Data_P", i, ".xlsx", sep = "")
  assign(nam, r_excel_names(Data_num)$class)
  
  nam <- paste("names_P", i, sep = "")
  Data_num <- paste("Data_P", i, ".xlsx", sep = "")
  assign(nam, r_excel_names(Data_num)$student)
  
  nam <- paste("date_P", i, sep = "")
  Data_num <- paste("Data_P", i, ".xlsx", sep = "")
  assign(nam, r_excel_names(Data_num)$date)
}


## Insert all Class P data in format DP3M : Data Pish 3, Math
for(i in 2:num_CP){
  for(j in 1:length(class_P3)){
    nam <- paste("DP", i,  class_label[j],  sep = "")
    Data_num <- paste("Data_P", i, ".xlsx", sep = "")
    assign(nam, r_excel_class(Data_num,j))
  }
}


### 
names_all <- c(names_P2,names_P3)
################
# Outside variables
################

CP2M_Mean <- round(colMeans(DP2M),digits = 2)
CP3M_Mean <- round(colMeans(DP3M),digits = 2)
