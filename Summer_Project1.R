library(foreign)
library(tidyverse)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(testit) # python-like assert statement
file_path = 'C:/Users/tnguy/OneDrive - Texas Tech University/Summer_Project/2012 NHIS Adult Sample_CAM_HIT paper 2017-2-17.sav'
###Load in dataset
data = read.spss(file_path, to.data.frame=TRUE)


### ALT data
###------
# top 3 alternative therapies
ALT = data %>% select('ALT_TP31', 'ALT_TP32', 'ALT_TP33')


### TP_REC
### ----
# top 3 alternative therapies; recommended by ...
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
cal_X_squ = function(data){
# The number of observations in the data
n = dim(data)[1]
# Matrices containing relative frequencies of yeses and nos count for each ith rec
yp_i = matrix(0, nrow=1,ncol=4)
np_i = matrix(0, nrow=1, ncol=4)
# Matrices containing count and relative frequencies of individuals who pick jth therapy
j_count=matrix(0,nrow=4,ncol=16)
p_j=matrix(0,nrow=1,ncol=16)
# Matrices containing counts and relative frequencies of individuals who pick jth therapy and ith rec
ij_count = matrix(0, nrow=4,ncol=16)
p_ij = matrix(0, nrow=4,ncol=16)
# Test statistics
ygrand_stats = 0
ngrand_stats = 0
# Iterate through each ith reccommendation
for (i in 1:4){
  # Initialize each ith's test stat with 0
  yes_stats =0
  no_stats = 0
  # Initialize each ith rec's yes and no count with zeros
  nn_i = 0
  yn_i = 0
  # Iterate through each observation to calculate test-statitstic
  for (indi in c(1:n)){
    # Need to refine missing data later
    # For now the denominator will equal 3
    ### The denominator varies according to the number of top choices each individual specifies
    yni_denominator = 3 #FOr this dataset, the denominator = 3
    # calculate the number of yes corresponding to the ith value of TP_REC for each individual
    yni_numerator=sum(c(i,i+4,i+8) %in% which(data[indi,1:12]==1))
    nni_numerator=sum(c(i,i+4,i+8) %in% which(data[indi,1:12]==2))
    # Calculate the proportion of yes and nos corresponding to the ith value of TP_REC for individuals in the data
    yn_i = yn_i + (yni_numerator / yni_denominator)
    nn_i = nn_i + (nni_numerator / yni_denominator)
    assert("Maximum proportion value must be less than or equal to 1", yni_numerator <= yni_denominator)
    # Iterate through the each jth therapy to compute each jth's count of individuals who pick yes
    for (j in data[indi,13:15]){
        j_count[i,j] = j_count[i,j] + 1
        # Calculate the frequency of individuals who choose ith rec and jth therapy
        if (sum(c(i,i+4,i+8) %in% which(data[indi,1:12]==1)) > 0){
          ij_count[i,j] = ij_count[i,j] + 1
        }
      }
    
  }
  # the grand total proportion of yeses and nos for the ith, jth and ijth values
  yp_i[1,i] = yn_i / n
  np_i[1,i] = 1 - (yn_i / n)
  # Relative frequencies of individuals who pick jth therapy and who pick jth therapy and ith rec
  p_j = j_count[1,] / n
  p_ij[i,] = ij_count[i,] / n
  # Calculate chisquare score 
  for (therap in (1:16)){
    yes_stats = yes_stats + (p_ij[i,j] - yp_i[1,i]*p_j[j])^2 / (yp_i[1,i]*p_j[j])
    no_stats = no_stats + (p_ij[i,j] - np_i[1,i]*p_j[j])^2 / (np_i[1,i]*p_j[j])
  }
  # The final results for X_M,1,1) and X_M,1,0)
  ygrand_stats = (ygrand_stats + yes_stats)
  ngrand_stats = (ngrand_stats + no_stats)
}
# return X_S
return(sum(n*ygrand_stats,n*ngrand_stats))
}
#Bootstrap method to calculate p_value (resample 1999 times)
cal_p_boot = function(data, X, B.max=1999, FUN=cal_X_squ){
  stat = FUN(data=data)
  X.sq.S.star = numeric(length(B.max))
  counter = 0
  b = 0
  while(b <= B.max){
    b = b+1
    n = dim(data)[1]
    end = dim(data)[2]
    W = sample(x=1:n,size=n,replace=TRUE)
    Y = sample(x=1:n,size=n,replace = TRUE)
    data.star = cbind(data[W,1:X],data[Y,(X+1):end])
    stat.star = FUN(data.star)
    
    counter = counter + 1
    X.sq.S.star[counter] = stat.star
  }
  p.value.boot = mean(X.sq.S.star >= stat)
  return(list(p_value=p.value.boot, X.sq=X.sq.S.star))
}

cal_p_boot(ATL_dropna, X=12)

# Simulate data to test the power of the statistical test
## ----
# new_data = matrix(0, nrow=500, ncol=6)

# Generate marginal probabilities for W_i
pi_R = c(0.4,0.6)
fun = function(p)((p/(0.4-p))/((0.5-p)/(0.1+p)) - 2)
pw.11 = uniroot(fun, lower=0.0, upper = 1.0)$root
pw.10 = 0.4 - pw.11
pw.01 = 0.5 - pw.11
pw.00 = 0.1 + pw.11
pw.11 + pw.10 + pw.01 + pw.00

# Generate marginal probabilities for Y_i
pi_C = c(0.2, 0.3)
py.11 = 0.085
py.10 = 0.2 - py.11
py.01 = pi_C[2] - py.11
py.00 = 0.5 + py.11
py.11 + py.00 + py.10 + py.01
# shuffle W - Alternative therapies probabilities
orderw = cbind(c(11,10,01,00),c(pw.11,pw.10,pw.01,pw.00))
orderw1 = orderw[sample(nrow(orderw)),]
orderw2 = cbind(orderw1, cumsum(orderw1[,2]))

# shuffle Y - Recommendation sources probabilities
ordery = cbind(c(11,10,01,00),c(py.11,py.10,py.01,py.00))
ordery1 = ordery[sample(nrow(ordery)),]
ordery2 = cbind(ordery1, cumsum(ordery1[,2])) 


#-------------------------------------
# Simulate data using computed marginal probabilities
simulated_data = matrix(NA, nrow=500, ncol=6)
colnames(simulated_data) = c('W1','W2','Y11','Y12','Y21','Y22')
generate_y = function(proby, data, Uy, row, pos1){
  #' @description Generate simulated data for the Y variable using specified probabilities
  #' @param proby matrix. A 1x3 matrix containing cumulative probability and its corresponding cluster
  #' @param data matrix. A 1x6 empty matrix to store data simulated for the Y variables
  #' @param Uy decimal. A decimal number randomly generated from a uniform distribution.
  #' @param pos1 integer. Integer numbers specifying columns to fill in data
  #' @param row integer. Interger number specifying the row to fill in data
  #' @usage generate_y(proby, data, Uy)
  #' @return a 500x6 matrix that contains simulated data from Y
  if (Uy >= proby[1,3] & Uy < proby[2,3]){
    data[row,pos1] = 0
    data[row, pos1+1] = 0
  } else if (Uy >= proby[2,3] & Uy < proby[3,3]){
    data[row, pos1] = 1
    data[row, pos1+1] = 0
  } else if (Uy >= proby[3,3] & Uy < proby[4,3]){
    data[row, pos1] = 1
    data[row, pos1+1] = 1 
  } else {
    data[row, pos1] = 0
    data[row, pos1+1] = 1
  }
  return(data)
}
generate_cluster = function(probw,proby, data, Uw, Uy1, Uy2, i, pos1, pos2, row){
#' @description Generate simulated data for W using spcified probabilities
#' @param probw,proby matrix. A 1x3 matrix containing cumulative probability and its corresponding cluster
#' @param data matrix. A 500x6 empty matrix to store simulated data
#' @param Uw,Uy1,Uy2 float. A float number generated from a uniform distribution
#' @param pos1,pos2 integer. Numbers specifying the columns to fill in the data with
#' @param i integer. A number specifying the row to fill in the data with
#' @usage generated_cluster(prob, data, U)
#' @return a 500x6 matrix that contains simulated data

  if (Uw >= probw[1,3] & Uw < probw[2,3]){
    data[i,1] = 0
    data[i,2] = 0
    data[i,3:6] = NA
  }
  else if (Uw >= probw[2,3] & Uw < probw[3,3]){
    data[i, 1] = 1
    data[i, 2] = 0
    data = generate_y(proby, data, Uy1,row=i, pos1)
  }
  else if (Uw >= probw[3,3] & Uw < probw[4,3]){
    data[i, 1] = 1
    data[i, 2] = 1
    data = generate_y(proby, data, Uy1, row=i, pos1)
    data = generate_y(proby, data, Uy2, row=i, pos2)
  }
  else {
   data[i,1] = 0
   data[i,2] = 1
   data = generate_y(proby, data, Uy2, row=i, pos2)
  }
  return (data)
}
simulate_data = function(n=500){
count = 0
while (count < n){
  Uw = runif(1, 0, 1)
  Uy1 = runif(1, 0, 1)
  Uy2 = runif(1, 0, 1)
  simulated_data = generate_cluster(probw = orderw2, proby = ordery2, Uw = Uw, Uy1=Uy1, Uy2=Uy2, i= count+1, pos1=3, pos2=5, data=simulated_data)
  count=count+1
}
# Change the order of columns to reflect the true order of the data due to the way Y's were generated
simulated_data[,c(1,2,3,4,5,6)] = simulated_data[,c(1,2,3,5,4,6)]
}

<<<<<<< HEAD
simulated_data = simulate_data()
=======

cal_p_boot = function(data, X, B.max=1999, FUN=cal_X_squ){
  stat = FUN(data=data)
  X.sq.S.star = numeric(length(B.max))
  counter = 0
  b = 0
  while(b <= B.max){
    b = b+1
    n = dim(data)[1]
    end = dim(data)[2]
    W = sample(x=1:n,size=n,replace=TRUE)
    Y = sample(x=1:n,size=n,replace = TRUE)
    data.star = cbind(data[W,1:X],data[Y,(X+1):end])
    stat.star = FUN(data.star)
    
    counter = counter + 1
    X.sq.S.star[counter] = stat.star
  }
  p.value.boot = mean(X.sq.S.star >= stat)
  return(list(p_value=p.value.boot, X.sq=X.sq.S.star))
}
dataprocess_stg2 = function(data){
  #' @description return nan for the Y variable if individuals who don't specify their W's choice
    
  }
}
>>>>>>> c33e67f9c7c6db2b020c3dc4ad13449d3000f7b1
