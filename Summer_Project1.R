library(foreign)
library(tidyverse)
file_path = 'C:/Users/tnguy/OneDrive - Texas Tech University/Summer_Project/2012 NHIS Adult Sample_CAM_HIT paper 2017-2-17.sav'
data = read.spss(file_path, to.data.frame=TRUE)


### ALT data
###------
ALT = data %>% select('ALT_TP31', 'ALT_TP32', 'ALT_TP33')


### TP_REC
### ----
Tp_REC = data %>% select('TP1_REC1', 'TP1_REC2', 'TP1_REC3', 'TP1_REC4')



### 1_INFF
### -----
TP_INF = data %>% select(TP1_INF1, TP1_INF2, TP1_INF3, TP1_INF4, TP1_INF5, TP1_INF6)




###----

### Select 2 factors of interest which are ALT_TP and TP_REC
ALT_Tp = data %>% select('TP1_REC1', 'TP1_REC2', 'TP1_REC3', 'TP1_REC4','TP2_REC1', 'TP2_REC2', 'TP2_REC3', 'TP2_REC4',
                         'TP3_REC1', 'TP3_REC2', 'TP3_REC3', 'TP3_REC4','ALT_TP31', 'ALT_TP32', 'ALT_TP33')

### Handling missing data
##----- 
#Drop NA from the data
ATL_dropna = na.omit(ALT_Tp)



## Catergorical Encoding
## ----
for (i in colnames(ATL_dropna)){
  ATL_dropna[i] = as.numeric(unlist(ATL_dropna[i]))
}

### Calculate chi
# 1 = Yes; 2 = No;
filter = which(ATL_dropna[,1:12] == 1, arr.ind=TRUE)
filter.df = as.data.frame(filter)
n = dim(ATL_dropna)[1]
for (i in 1:4){
  
  for (j in 1:3){
    
  }
}


for (i in ATL_dropna[1,1:12]){
  print(i)
}
count = 0
for (i in unique(filter.df$row)){
  count = count +1
  print(max(filter.df[filter.df$row==i,]$col))
}
