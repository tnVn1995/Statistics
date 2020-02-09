simulated_Xsqu = function(data, therap, rec, cond = c(1,1), simulated=TRUE){
  #' @description Calculate modified Pearson statistic 
  #' @param data matrix. the data to perform calculation on. The data has to be in binary form of either 1 or 0
  #' @param therap integer. the number of therapies
  #' @param rec integer. the number of recommendation sources
  #' @param cond vector-like. Pairs of choices for each individual. If simulated=TRUE, the possible combinations of pairs are (1,1), (1,0), (0,1), (1,1). 
  #' 1 means yes while 0 means no. Otherwise, pairs would be (integer, 1) or (integer, 0). integer denotes the therapy choice while 1 or 0 denotes rec choice.
  #' @param simulated Boolean. If the data provided is the real data or the simulated data 
  n = dim(data)[1]
  i_count= matrix(0, nrow=1, ncol=therap)
  pi_i.= matrix(0, nrow=1, ncol=therap)
  # ij count matrices
  ij_count = matrix(0, nrow=therap, ncol = rec)
  # notij_count = matrix(0, nrow=therap, ncol = rec)
  y_ij = matrix(0, nrow=therap, ncol=rec)
  # n_ij = matrix(0, nrow=therap, ncol=rec)
  # The count of individuals who pick jth recommendation source
  y_nj = matrix(0,nrow=1,ncol=rec)
  # n_nj = matrix(0,nrow=1,ncol=rec)
  # The prob estimation of y_nj and n_nj
  yp_.j =  matrix(0,nrow=1,ncol=rec)
  # np_.j =  matrix(0,nrow=1,ncol=rec)
  # no = 2 #dimension of the matrix
  if (simulated == TRUE){
    for (indi in 1:n){
      for (i in 1:therap){
        if (data[indi,i]!=cond[1]) # CHECK FOR ZERO CONDITION TOO
          next
        else {
          # Counting the number of individuals who pick jth therapy to calculate pi_.i
          i_count[,i] = i_count[,i] + 1
          # Iterating through each therapy, count the participants who pick the jth recommendation to calculate pi_ij
          for (j in 1:rec){
            if (data[indi,therap+i+(j-1)*rec]==cond[2]){
              ij_count[i,j] = ij_count[i,j] + 1
            }
            # else {
            #   notij_count[i,j] = notij_count[i,j] + 1
            #   # Remember to check to sum of ij_count 
            # }
            
          }
          
        }
      }
      # for the real dataset, use sum of not nan values
      y_denominator = sum(data[indi,1:therap]==1) #The number of therapies specified
      if (y_denominator == 0){
        sprintf('The %sth individual does not specify their therapy pick', i)
        sprintf('Skip this individual')
        next()
      }
      else {
        for (j in 1:rec){
          # Count the individuals who pick jth recommendation source
          # Only count when that individual specifies their ith therapy pick
          # first remove na from consideration
          ycond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==cond[2]
          ncond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==!cond[2]
          # the numerator is the number of y's or no's 
          ynj_numerator = sum(ycond[,which(!is.na(ycond))])
          nnj_numerator = sum(ncond[,which(!is.na(ncond))])
          assert('should not be nan values', !is.na(ynj_numerator))
          # assert('Should not be nan values', !is.na(nnj_numerator))
          assert("Your .j count is not correct!", ynj_numerator + nnj_numerator == y_denominator)
          # Count the individuals who specify their jth recommendation source choice
          y_nj[,j] = y_nj[,j] + ynj_numerator / y_denominator
          # n_nj[,j] = n_nj[,j] + nnj_numerator / y_denominator
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
    grandstat = yp_grandstat * n
  }
  else {
  for (indi in 1:n){
    for (i in 1:therap){
      if (data[indi,i]!=cond[1])
        next
      else {
        # Counting the number of individuals who pick jth therapy to calculate pi_.i
        i_count[,i] = i_count[,i] + 1
        # Iterating through each therapy, count the participants who pick the jth recommendation to calculate pi_ij
        for (j in 1:rec){
          if (data[indi,therap+i+(j-1)*rec]==cond[2]){
            ij_count[i,j] = ij_count[i,j] + 1
          }
          # else {
          #   notij_count[i,j] = notij_count[i,j] + 1
          #   # Remember to check to sum of ij_count 
          # }
          
        }
        
      }
    }
    y_denominator = sum(data[indi,1:therap]==1) #The number of therapis specified
    #assert("Cannot be zero", y_denominator != 0)
    if (y_denominator == 0){
      sprintf('The %sth individual does not specify their therapy pick', i)
      sprintf('Skip this individual')
      next()
    }
    else {
      for (j in 1:rec){
        # Count the individuals who pick jth recommendation source
        # Only count when that individual specifies their ith therapy pick
        # first remove na from consideration
        ycond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==cond[2]
        ncond = data[indi,(therap+1+(j-1)*rec):(therap+ j*rec)]==!cond[2]
        # the numerator is the number of y's or no's 
        ynj_numerator = sum(ycond[,which(!is.na(ycond))])
        nnj_numerator = sum(ncond[,which(!is.na(ncond))])
        assert('should not be nan values', !is.na(ynj_numerator))
        # assert('Should not be nan values', !is.na(nnj_numerator))
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
  grandstat = (yp_grandstat + np_grandstat) * n
  
  }
  return(grandstat)
}

grandstat = simulated_Xsqu(simulated_data, therap=2, rec=2)