counts = matrix(0.00,nrow=1,ncol=100)
# for (count in 1:100){

i_count= matrix(0, nrow=1, ncol=therap)
ij_count = matrix(0, nrow=therap, ncol = rec)
notij_count = matrix(0, nrow=therap, ncol = rec)

# start1 = proc.time()
for (indi in 1:n){
  for (i in 1:therap){
    if (data[indi,i]==0)
      next
    else {
      # Counting the number of individuals who pick jth therapy to calculate pi_.i
      i_count[,i] = i_count[,i] + 1
      # Iterating through each therapy, count the participants who pick the jth recommendation to calculate pi_ij
      for (j in 1:rec){
        if (data[indi,therap+i+(j-1)*rec]==1){
          ij_count[i,j] = ij_count[i,j] + 1
        }
        else {
          notij_count[i,j] = notij_count[i,j] + 1
          # Remember to check to sum of ij_count 
        }
        
      }
      
    }
  }
}
# }
# end1 = proc.time()
# print(end1-start1)
# counts[,count] = (end1-start1)[3]
}
#----
count1s = matrix(0.00, nrow=1,ncol=100)
# for (count1 in 1:100){
getij = function(data, therap, rec){
  i_count1= matrix(0, nrow=1, ncol=therap)
  ij_count1 = matrix(0, nrow=therap, ncol = rec)
  notij_count1 = matrix(0, nrow=therap, ncol = rec)
# start2 = proc.time()
for (i in 1:therap){
  i_count1[,i] = length(which(data[,i]==1))
  yind = which(data[,i]==1)
  for (j in 1:rec){
  for (yindi in yind){
        if (data[yindi,therap+i+(j-1)*rec]==1){
          ij_count1[i,j] = ij_count1[i,j] + 1
        }
    else{
        notij_count1[i,j] = notij_count1[i,j] + 1
    }
      }
      }
    }
return(list(i_count=i_count1, ij_count=ij_count1, notij_count=notij_count1))
}
end2 = proc.time()
# print((end1-start1)[3])
# count1s[,count1] = (end2-start2)[3]
# }

###------------------------------------------------------------------------------------------
therap = 2
rec = 2
therap=therap
rec=rec
n=500
y_ij = matrix(0, nrow=therap, ncol=rec)
n_ij = matrix(0, nrow=therap, ncol=rec)
y_nj = matrix(0,nrow=1,ncol=rec)
n_nj = matrix(0,nrow=1,ncol=rec)
for (indi in 1:n){
y_denominator = sum(data[indi,1:therap]==1)
#assert("Cannot be zero", y_denominator != 0)
if (y_denominator == 0){
  next()
}
else {
  for (j in 1:rec){
    # Count the individuals who pick jth recommendation source
    # Only count when that individual specifies their ith therapy pick
    # first remove na from consideration
    ycond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==1
    ncond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==0
    # the numerator is the number of y's or no's 
    ynj_numerator = sum(ycond[which(!is.na(ycond))])
    nnj_numerator = sum(ncond[which(!is.na(ncond))])
    assert('should not be nan values', !is.na(ynj_numerator))
    assert('Should not be nan values', !is.na(nnj_numerator))
    assert("Your .j count is not correct!", ynj_numerator + nnj_numerator == y_denominator)
    # Count the individuals who specify their jth recommendation source choice
    y_nj[,j] = y_nj[,j] + ynj_numerator / y_denominator
    n_nj[,j] = n_nj[,j] + nnj_numerator / y_denominator
  }
}
}
###------------------------------------------------------------------------------------------
temp_X_cal= function(data, therap, rec){
  # In this dataset, 1 means yes and 0 means no
  therap=therap
  rec=rec
  n=500
  # i count matrices
  i_count= matrix(0, nrow=1, ncol=therap)
  pi_i.= matrix(0, nrow=1, ncol=therap)
  # ij count matrices
  ij_count = matrix(0, nrow=therap, ncol = rec)
  notij_count = matrix(0, nrow=therap, ncol = rec)
  # The count of individuals who pick jth recommendation source
  y_nj = matrix(0,nrow=1,ncol=rec)
  n_nj = matrix(0,nrow=1,ncol=rec)
  # The prob estimation of y_nj and n_nj
  yp_.j =  matrix(0,nrow=1,ncol=rec)
  np_.j =  matrix(0,nrow=1,ncol=rec)
  no = 2 #dimension of the matrix
  for (indi in 1:n){
    for (i in 1:therap){
      if (data[indi,i]==0)
        next
      else {
        # Counting the number of individuals who pick jth therapy to calculate pi_.i
        i_count[,i] = i_count[,i] + 1
        # Iterating through each therapy, count the participants who pick the jth recommendation to calculate pi_ij
        for (j in 1:rec){
          if (data[indi,therap+i+(j-1)*rec]==1){
            ij_count[i,j] = ij_count[i,j] + 1
          }
          else {
            notij_count[i,j] = notij_count[i,j] + 1
            # Remember to check to sum of ij_count 
          }
          
        }
        
      }
    }
    # for the real dataset, use sum of not nan values
    y_denominator = sum(data[indi,1:therap]==1)
    #assert("Cannot be zero", y_denominator != 0)
    if (y_denominator == 0){
      next()
    }
    else {
      for (j in 1:rec){
        # Count the individuals who pick jth recommendation source
        # Only count when that individual specifies their ith therapy pick
        # first remove na from consideration
        ycond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==1
        ncond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==0
        # the numerator is the number of y's or no's 
        ynj_numerator = sum(ycond[which(!is.na(ycond))])
        nnj_numerator = sum(ncond[which(!is.na(ncond))])
        assert('should not be nan values', !is.na(ynj_numerator))
        assert('Should not be nan values', !is.na(nnj_numerator))
        assert("Your .j count is not correct!", ynj_numerator + nnj_numerator == y_denominator)
        # Count the individuals who specify their jth recommendation source choice
        y_nj[,j] = y_nj[,j] + ynj_numerator / y_denominator
        n_nj[,j] = n_nj[,j] + nnj_numerator / y_denominator
      }
    }
  }
  # Sanity check of the count
  assert("The number of ijth pick is not correct!", ij_count+notij_count == matrix(rep(colSums(data[,c(1:therap)]),times=rec), nrow=therap, ncol=rec))
  assert('THe number of ith pick is not correct!', i_count == colSums(data[,c(1:therap)]))
  pi_i. = i_count / n
  # the grand total proportion of yeses and nos for the ith, jth and ijth values
  yp_.j = y_nj / n
  np_.j = n_nj / n
  ypi_ij = ij_count / n
  npi_ij = notij_count / n
  # Grand-test stat
  yp_grandstat = sum((ypi_ij - t(pi_i.) %*% yp_.j)^2 / (t(pi_i.) %*% yp_.j))
  np_grandstat = sum((npi_ij - t(pi_i.) %*% np_.j)^2 / (t(pi_i.) %*% np_.j))
  return (list(grandstat = (yp_grandstat + np_grandstat) * n, yes_stat = yp_grandstat, no_stat = np_grandstat))
}
##----
# Test grond
# p_i. = matrix(c(3,4),nrow=1,ncol=2)
# p_.j = matrix(c(2,3),nrow=1,ncol=2)
# p_ij = matrix(c(5,6,7,8), nrow=2,ncol=2,byrow = TRUE)
# pi.pj = t(p_i.) %*% p_.j
# sum((p_ij - pi.pj)^2 / pi.pj)
# sumTest = 0
# for (i in 1:2){
#   print( p_i.[,i])
#   print('--')
#   for (j in 1:2){
#     print(p_.j[,j])
#     print(p_ij[i,j])
#     print(p_i.[,i] * p_.j[,j])
#     print('++')
#     sumTest = sumTest + (p_ij[i,j] - p_i.[,i] * p_.j[,j])^2 / (p_i.[,i] * p_.j[,j])
#   }
# }