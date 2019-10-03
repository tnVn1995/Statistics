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
  p_i = 0
  for (indi in c(1:2091)){
  if (max(which(ATL_dropna[indi,1:12]==1)) >= 9){
    pi_denumerator = 3
    }
  else if (5 <= max(which(ATL_dropna[indi,1:12]==1)) < 9){
    pi_denumerator = 2
    }
  else {
    pi_denumerator = 1
  }
  pi_numerator=sum(c(i,i+4,i+8) %in% which(ATL_dropna[indi,1:12]==1))
  p_i = p_i + (pi_numerator / pi_denumerator)  
  }
  for (j in 1:3){
    
  }
}

j_count=matrix(0,nrow=1,ncol=16)
for (indi in c(1:2019)){
  for (j in ATL_dropna[indi,13:15]){
  j_count[1,j] = j_count[1,j] + 1
  }
}
j_count = j_count / 2091



for (i in ATL_dropna[1,13:15]){
  print(i)
}




