#######################################################################
### All rights are reserved by the authors.
### FRM_Quantile_Regression has been created by Andrija Mihoci
### FRM_Quantile_Regression (ENET) has been adjusted by Alice Drube
### Authors: Youjuan Li, Ji Zhu, University of Michigan (jizhu@umich.edu)
###          Martin Slawski, George Mason University              
### Date:    07/01/2006
#######################################################################


FRM_Quantile_Regression = function(Xd, j, a, max.steps, maxloop = M_t){ #for simplicity I omitted the ENET in the name
  y = as.matrix(Xd[, j])
  x = Xd[, -j]
  Lambda2 = 5  #Lambda2 fixed for the calculation of lambda1; choose any value: 3.5
  eps1 = 10^(-10)
  eps2 = 10^(-6)
  n = dim(x)[1]
  
  m = dim(x)[2]
  maxvars = min(m, n - 1)
  indm = seq(m)
  indn = seq(n)
  
  Cgacv   = double(max.steps + 1)
  checkf  = rep(0, max.steps + 1)
  Csic    = double(max.steps + 1)
  fit     = matrix(0, max.steps + 1, n)
  
  L1 <- FRM_Quantile_Regression_ENET_L1(y, x, a, Lambda2, max.steps, eps1, eps2, trace = T)
  
  #calculating L2 with fixed L1
  nL1        = length(L1$Lambda1[L1$Lambda1 > 0])
  nE         = rep(0, nL1)
  nStep      = rep(0, nL1)
  optLam2    = rep(0, nL1)
  optLam2SIC = rep(0, nL1)
  beta0      = rep(0, nL1)
  beta       = matrix(0, nL1, m)
  optTheta   = matrix(0, nL1, n)
  
  for(index in 1:nL1){
    #cat("Index = ", index,"\n")
    L2 <- FRM_Quantile_Regression_ENET_L2(y, x, a, input = L1, i = index, Lambda2, max.steps, 
                                          maxind = nL1, atLoop = j, maxloop, eps1, eps2)
    
    ### select Lambda2 with the min. GACV and its corresponding coefficients
    nStep[index]     = L2$Step
    indCGACV         = which(L2$CGACV == min(L2$CGACV))
    if(length(indCGACV) > 1)
      indCGACV       = indCGACV[length(indCGACV)]
    beta[index,]     = L2$Beta[indCGACV,]
    beta0[index]     = L2$Beta0[indCGACV]
    nE [index]       = length(L2$Elbow.List[[indCGACV]])
    optLam2[index]   = L2$Lambda2[indCGACV]
    optTheta[index,] = L2$Theta[indCGACV,]
  }
  
  L1$Lambda1  = L1$Lambda1[L1$Lambda1 > 0]
  lambda      = L1$Lambda1/(L1$Lambda1 + optLam2/2)
  LAM         = optLam2/2 + L1$Lambda1
  Res         = matrix(0, length(beta0), n)
  
  for(i in 1:length(beta0)){
    Res[i,]   = y - drop(x %*% beta[i,] + beta0[i])
    trHat     = nE[i]
    checkf[i] = sum(optTheta[i,] * Res[i,])
    Cgacv[i]  = checkf[i] / (n-trHat)
    Csic[i]   = log(checkf[i]/n) + (log(n)/(2*n))*trHat
  }
  
  Cgacv  = Cgacv[Cgacv > 0]
  LAM    = LAM[Cgacv > 0]
  lambda = lambda[Cgacv > 0]
  
  # ## select only scenarios with step > 1
  # if(sum(nStep > 1) > 0){
  # Cgacv = Cgacv[which(nStep > 1)]
  # LAM = LAM[which(nStep > 1)]
  # beta = beta[which(nStep > 1),]
  # lambda = lambda[which(nStep > 1)]
  # }
  # else{
  #   Cgacv = Cgacv[Cgacv > 0]
  # }

  # take LAM instead of lambda
  #lambda = LAM
  
  object <- list(beta0 = beta0, beta = beta, lambda = LAM, Csic=Csic, Cgacv=Cgacv, lambdatest = lambda)
  object
}
### Function 'FRM_Quantile_Regression_ENET_L1':
### computes the entire path for the elastic net quantile regression with fixed lambda2
### y in Real set 
### a_th quantile
### eps1: numerical
### eps2: lambda limit , eps2 = 10^(-6) not needed?
FRM_Quantile_Regression_ENET_L1 = function(y, x, a, Lambda2, max.steps, eps1 = 10^(-10), eps2, trace = T) {
  
  # ### SETUP ###
  # y = as.matrix(Xd[, j]) 
  # x = Xd[, -j]
  
  n = dim(x)[1]
  m = dim(x)[2]
  maxvars = min(m, n - 1)
  indm = seq(m)
  indn = seq(n)
  
  ### Guarantee distinct y's even for tied data
  tmpy = unique(y)            
  if (length(tmpy) < length(y)) {
    dif = min(diff(sort(tmpy)))
    y = y + rnorm(length(y), 0, dif/100)
  }
  
  ### lambda save the -lambda values for all steps
  call = match.call()
  
  ###first column is for s=0, 2nd column is for the 1st event
  beta      = matrix(0, max.steps + 1, m)
  beta0     = rep(0, max.steps + 1)
  lambda1   = rep(0, max.steps + 1)
  sdistance = rep(0, max.steps + 1)
  theta.g   = matrix(0, max.steps + 1, n)
  
  E.list    = as.list(seq(max.steps + 1))
  L.list    = as.list(seq(max.steps + 1))
  R.list    = as.list(seq(max.steps + 1))
  Res.list  = as.list(seq(max.steps + 1)) #neu am 23.4.
  Sign.list = as.list(seq(max.steps + 1))
  V.list    = as.list(seq(max.steps + 1))
  
  ### Initial Conditions
  ini_enet  = qrenetini(x, y, a)
  
  indE = ini_enet$indE      ### indE: index of the points in the elbow
  indL = ini_enet$indL      ### indL: index of the points left of the elbow
  indR = ini_enet$indR      ### indR: index of the points right of the elbow
  indV = ini_enet$indV      ### indV: index of the active set, beta_j is nonzero
  beta0[1] = ini_enet$beta0
  lambda1[1] = ini_enet$lambda
  Sign = ini_enet$sign 
  res = ini_enet$residual   ### residual: y-f, should be 0 in the elbow
  theta.g[1,] = ini_enet$theta 
  corr = ini_enet$xprod
  E.list[[1]]    = indE
  L.list[[1]]    = indL
  R.list[[1]]    = indR
  Res.list[[1]]  = res #neu am 23.4.
  Sign.list[[1]] = Sign
  V.list[[1]]    = indV
  ### Initialization done
  ######################
  
  ### Main loop for path
  k = 0
  while((k < max.steps)  & (lambda1[k + 1] > eps1)) {
    k = k + 1
    
    #cat("step:",k,"\n")
    if(length(indE) == 0)
      stop("no points in the Elbow \n")
    
    tmpA = matrix(0, nrow = length(indE) + 1, ncol = length(indE) + 1)
    
    tmpA[1:length(indE), 1] = Lambda2 
    tmpA[1:length(indE), 2:(length(indE) + 1)] = x[indE, indV, drop = F] %*% t(x[indE, indV, drop = F])
    tmpA[length(indE) + 1, 2:(length(indE) + 1)] = 1
    tmpb = c(-x[indE, indV, drop = F] %*% Sign[indV], 0)
    
    tmpqr = qr(tmpA)
    if (tmpqr$rank < (length(indE) + 1)){
      stop("The matrix is singular.  Can not get derivatives. \n")
    }else{
      tmpsol = array(qr.solve(tmpqr, tmpb))
    }
    
    d_beta0 = tmpsol[1]
    d_theta = tmpsol[2:(length(indE) + 1)]
    d_beta = (t(x[indE, indV, drop = F]) %*% d_theta + Sign[indV])/Lambda2
    if(length(d_beta) == 1){
      ident = rep(1, m - length(indV))
      d_corr = ident * t(x[indE, -indV, drop = F]) %*% d_theta - ident %*% (Lambda2 * d_beta)
    }else{
      ident = rep(1, m - length(indV))
      identMat = matrix(1, m - length(indV), length(d_beta))
      d_corr = ident * t(x[indE, -indV, drop = F]) %*% d_theta - identMat %*% (Lambda2 * d_beta)
    }
    d_res = -(d_beta0 + array(x[-indE, indV, drop = FALSE] %*% as.matrix(d_beta)))
    d_res[abs(d_res) < eps1] = 0
    
    
    ### Determination of the stepsize ###
    ### different scenarios ###
    
    ### an elbow point leaves the elbow
    if(any(theta.g[k,indE] >= (a - eps2) & d_theta > 0) |
       any(theta.g[k,indE] <= (-(1 - a) + eps2) & d_theta < 0)){
      del1sE = pmax((-(1 - a) - theta.g[k,indE])/d_theta, (a - theta.g[k,indE])/d_theta)
      del1s = 0
    }else{
      del1sE = pmax((-(1 - a) - theta.g[k,indE])/d_theta, (a - theta.g[k,indE])/d_theta)
      del1sE[is.na(del1sE)] <- Inf
      if(length(del1sE[del1sE > 0]) == 0){ del1s <- Inf
      }else{del1s <- min(del1sE[del1sE > 0])}
    }
    if(del1s < 0) stop("Non-positive stepsize E -> L/R \n")
    
    
    ### a non-elbow point hits the elbow
    del2sLR <- rep(NA, n)
    del2sLR[-indE] <- (0 - res[-indE])/d_res
    del2sLR[is.na(del2sLR)] <- Inf
    if(length(del2sLR[del2sLR > 0]) == 0){del2s <- Inf
    }else{del2s <- min(del2sLR[del2sLR > 0]) } 
    
    if(del2s <= 0) stop("Non-positive stepsize transition L/R -> E \n")
    
    ### an active variable becomes inactive
    del3sVc <- (0 - beta[k, indV])/d_beta
    del3sVc[is.na(del3sVc)] <- Inf
    if(length(del3sVc[del3sVc > 0]) == 0){del3s <- Inf
    }else{ del3s <- min(del3sVc[del3sVc > 0])   }
    
    if(del3s <= 0) stop("Non-positive stepsize transition V -> V^c \n")
    
    ### an inactive variable joins the active set
    if(m - length(indV) > 0){
      del4sV <- cbind((-(lambda1[k] + corr[-indV]))/(d_corr - 1), (lambda1[k] - corr[-indV])/(d_corr + 1))
      del4sV[del4sV < 0 & del4sV > -eps1] <- 0
      del4sV[del4sV < 0] <- Inf
      ###
      del4s <- min(apply(del4sV, 1, min))
      
      if(del4s < 0) stop("Non-positive stepsize transition V^c -> V \n")
    }else{del4s <- Inf }
    
    del5s = lambda1[k]
    
    deltas = c(del1s, del2s, del3s, del4s, del5s)
    delta = min(deltas)
    event = which(deltas == delta)
    #cat("\t the deltas are ", deltas ," & event", event,"takes place\n")
    if(length(event) > 1) cat("\t multiple events in \n")
    
    #change order with theta.g
    beta0[k+1] = beta0[k] + delta * d_beta0
    beta[k+1, indV] = beta[k, indV] + delta * d_beta
    beta[k+1, indV][abs(beta[k + 1, indV]) < eps1] = 0
    lambda1[k + 1] = lambda1[k] - delta
    res[-indE] <- res[-indE] + delta * d_res
    
    ### Update the different quantities
    theta.g[k + 1, ] = theta.g[k,]
    theta.g[k + 1, indE] <- theta.g[k, indE] + delta * d_theta
    if(any(theta.g[k + 1, indE] < -(1 - a))){
      theta.g[k + 1, indE][theta.g[k + 1, indE] < -(1 - a)] = -(1 - a) #if a point moves from E to L
    }else if(any(theta.g[k + 1, indE] > a)){
      theta.g[k + 1, indE][theta.g[k + 1, indE] > a] = a #if a point moves from E to R
    }
    
    if(any(res[indL] > eps1)) stop("Positive residual in L \n")
    if(any(res[-indL] < -eps1)) stop("Negative residual in E/R \n") 
    
    res[-indE][abs(res[-indE]) < eps1] <- 0
    
    sdistance[k+1] = sdistance[k] + delta * sum(abs(d_beta))
    
    
    ### Update of the different status sets ###
    
    if(!is.na(match(1, event))){
      indoutEPnt <- which(del1sE == min(del1sE), arr.ind = TRUE)
      for(j in 1:length(indoutEPnt)){
        if (abs(theta.g[k + 1, indE[indoutEPnt[j]]] - a) < eps1 |
            theta.g[k + 1, indE[indoutEPnt[j]]] == a){
          indR <- sort(c(indR, indE[indoutEPnt[j]]))
          theta.g[k + 1, indE[indoutEPnt[j]]] <- a
        }else{
          indL <- sort(c(indL, indE[indoutEPnt[j]]))
          theta.g[k + 1, indE[indoutEPnt[j]]] <- -( 1 - a)
        }
      }
      #cat("\t point(s)", indE[indoutEPnt],"are removed from the elbow \n")
      indE <- indE[-indoutEPnt]
    }
    
    if(!is.na(match(2, event))){
      indnewEPnt <- which(del2sLR == del2s, arr.ind = TRUE)
      indE <- c(indE, indnewEPnt)
      for(j in 1:length(indnewEPnt)){
        if(sum(!is.na(match(indL, indnewEPnt[j]))) > 0){
          indL <- indL[-which(indL == indnewEPnt[j])]
        }  
        if(sum(!is.na(match(indR, indnewEPnt[j]))) > 0){
          indR <- indR[-which(indR == indnewEPnt[j])]
        }
      } 
      #cat("\t point", indnewEPnt, "joins the elbow \n")
    }
    
    if(!is.na(match(3, event))){
      removedVar <- which(del3sVc == del3s, arr.ind = TRUE)[1]
      indV <- indV[-removedVar]
      for(j in 1:length(removedVar)){
        Sign[removedVar[j]] <- 0 
      }
    }
    
    if(!is.na(match(4, event))){
      indnewVar <- which(del4sV == del4s, arr.ind = TRUE)
      newVar <- which(dimnames(x)[[2]] == dimnames(indnewVar)[[1]])
      indV <- c(indV, newVar)
      for(j in 1:length(newVar)){ 
        if(del4s == del4sV[indnewVar[j],1]){
          Sign[newVar[j]] <- -1
        }     
        if(del4s == del4sV[indnewVar[j],2]){
          Sign[newVar[j]] <- 1
        }
      } 
    }
    
    if(any(event == 5)){ lambda1[k + 1] <- 0}
    
    ### update generalized correlations
    corr[indV] <- lambda1[k + 1] * Sign[indV]
    corr[-indV] <-  drop(t(x[, -indV, drop = FALSE]) %*% theta.g[k + 1,]  - 
                           rep(t(rep(Lambda2, length(indV))) %*% beta[k + 1, indV], m - length(indV)))  
    
    E.list[[k + 1]] <- indE
    V.list[[k + 1]] <- indV
    R.list[[k + 1]] <- indR
    L.list[[k + 1]] <- indL
    Sign.list[[k + 1]] <- Sign 
    Res.list[[k+1]] <- res
  }
  
  #check for comment - won't be needed unless lambda2 is calculated
  # ### Update criteria SIC and GACV
  # checkfct[k + 1] = sum(theta.g[k + 1,] * res)
  # trHat = 0 # das hier wird anders berechnet
  # Cgacv[k + 1] = checkfct[k + 1] / (n-trHat)
  # Csic[k + 1] = log(checkfct[k + 1]/n) + (log(n)/(2*n))*trHat
  
  
  object = list(beta0 = beta0, beta = beta, Elbow.list = E.list[seq(k+1)], L.list = L.list[seq(k+1)], Lambda1 = lambda1, 
                Lambda2 = Lambda2, R.list = R.list[seq(k+1)], Res.list = Res.list, s = sdistance[seq(k+1)], 
                Sign.list = Sign.list[seq(k+1)], Theta = theta.g, V.list = V.list[seq(k+1)])
  object
}

### Initialization 
qrenetini = function(x, y, a, eps=10^(-10)) {
  
  ### y in Real set
  ### eps is defined as nonzero
  ### a_th quantile
  ### lambda is "-lambda"
  n = dim(x)[1]
  m = dim(x)[2]
  
  #Determine Beta0
  ### Two cases 
  yr       = sort(y)
  tauindx  = n * a
  if(!is.integer(tauindx) == tauindx){
    beta0  = yr[floor(tauindx) + 1]
  } else {
    beta0  = yr[tauindx] #setting beta0 equal to the left endpoint 
  }
  
  indx     = match(beta0, y)
  if(length(indx) > 1)
    stop("Error: There should only be one point in the elbow. \n")
  indm     = seq(m)
  E        = indx
  R        = seq(y)[y > y[indx]]
  L        = seq(y)[y < y[indx]]
  
  theta    = rep(0,n)
  theta[L] = -(1-a)
  theta[R] = a
  theta[E] = -sum(theta)
  
  residual = y - beta0
  
  #compute max. abs. sum of theta_i * x_ij to find init. V that corresponds to largest feasible lambdastar
  xprod = drop(t(as.matrix(theta)) %*% x) 
  lambda = max(abs(xprod))
  V = indm[which.max(xprod)]
  sign = rep(0,m)
  sign[V] = sign(xprod[V])
  
  return(list(beta0 = beta0 ,lambda = lambda, sign = sign, theta = theta, xprod = xprod,
              indV = V, indE = E, indR = R, indL = L, residual = residual))
}

### Function 'FRM_Quantile_Regression_ENET_L2':
### solution path in 0.5 * \lambda_2 \beta^{\T} \beta.
###   X:        design matrix, without intercept. 
###   input:   result from call to 'FRM_Quantile_Regression_ENET_L1'
###   index:    Use the solution of 'input' after 'index' steps as entry point for the 
###             solution path in 0.5 * \lambda_2 \beta^{\T} \beta.
###   maxStep, error: s. 'FRM_Quantile_Regression_ENET_L1'.
FRM_Quantile_Regression_ENET_L2 <- function(y, x, a, Lambda, input, i, max.steps, 
                                            maxind, atLoop, maxloop, savePath=TRUE, eps1 = 10^(-10), eps2 = 10^(-6)){
  
  n = dim(x)[1]
  m = dim(x)[2]
  indm = seq(m)
  indn = seq(n)
  nL1 = maxind
  
  beta      = matrix(0, max.steps + 1, m)
  beta0     = rep(0, max.steps + 1)
  ObjVal    = rep(0, max.steps + 1)
  Sign.list = as.list(seq(max.steps + 1))
  theta.g   = matrix(0, max.steps + 1, n)
  E.list    = as.list(seq(max.steps + 1))
  L.list    = as.list(seq(max.steps + 1))
  R.list    = as.list(seq(max.steps + 1))
  V.list    = as.list(seq(max.steps + 1))
  
  Cgacv     = rep(0, max.steps + 1)
  checkfct  = rep(0, max.steps + 1)
  Csic      = rep(0, max.steps + 1)
  fit       = matrix(0, max.steps + 1, n)
  
  lambda2 = rep(0, max.steps + 1)
  lambda2[1] <- input$Lambda2
  
  beta0[1] <- input$beta0[i]
  beta[1,] <- input$beta[i,]
  E.list[[1]] = indE = input$Elbow.list[[i]]
  L.list[[1]] = indL = input$L.list[[i]]
  R.list[[1]] = indR = input$R.list[[i]]
  V.list[[1]] = indV = input$V.list[[i]]
  lambda1 <- input$Lambda1[i]
  Sign <- numeric(ncol(x))
  Sign.list[[1]] <- input$Sign.list[[i]]
  Sign[indV] <- input$Sign.list[[i]][indV] 
  
  theta.g[1,] <- input$Theta[i,]
  
  ### compute residual and do some checks 
  res <- y - drop(x %*% beta[1,] + beta0[1])
  Res <- res
  Res[abs(Res) < eps1] <- 0
  ObjVal[1] <- sum(a * Res[Res > 0]) - (1 - a) * sum(Res[Res < 0])
  
  
  ### compute the check function for variable selection
  checkfct[1] = sum(theta.g[1,] * res)
  trHat = length(indE)
  Cgacv[1] = checkfct[1] / (n-trHat)
  Csic[1] = log(checkfct[1]/n) + (log(n)/(2*n))*trHat
  
  allPredIncluded <- FALSE
  perfectlyseparated <- FALSE
  
  k = 0
  
  while ( (k < max.steps) & (lambda2[k + 1] > eps1) ){ 
    
    ### 1: compute d_alpha
    k = k + 1
    #cat("step:",k,"\n")

    if(length(indE) == 0){
      #cat("no points in the Elbow \n")
      break}
    
    tmpA = matrix(0, nrow = length(indE) + 1, ncol = length(indE) + 1)
    
    tmpA[1:length(indE), 1] = rep(1, length(indE)) 
    tmpA[1:length(indE), 2:(length(indE) + 1)] = x[indE, indV, drop = F] %*% t(x[indE, indV, drop = F])
    tmpA[length(indE) + 1, 2:(length(indE) + 1)] = 1
    tmpb = c(y[indE], 0)
    
    tmpqr = qr(tmpA)
    if (tmpqr$rank < (length(indE) + 1)){
      #cat("Matrix is singular -> No derivatives.  \n")
      break
    }else{
      tmpsol = array(qr.solve(tmpqr, tmpb))
    }
    
    d_theta0   = tmpsol[1]
    d_theta    = tmpsol[2:(length(indE) + 1)]
    theta0     = lambda2[k] * beta0[k]  
    lambeta    = drop(t(x[,indV, drop = FALSE]) %*% theta.g[k,] - lambda1 * Sign[indV])
    d_lambeta  = drop(t(x[indE,indV, drop = FALSE]) %*% d_theta)
    
    
    ####################################################################################
    #### determine stepsize 
    ####################################################################################
    
    ### an elbow point leaves the elbow
    if(any(theta.g[k,indE] >= (a - eps2) & d_theta < 0) |
       any(theta.g[k,indE] <= (-(1 - a) + eps2) & d_theta > 0)){
      del1sE = pmin((-(1 - a) - theta.g[k,indE])/d_theta, (a - theta.g[k,indE])/d_theta)
      del1s = -eps1
    }else{
      del1sE = pmin((-(1 - a) - theta.g[k,indE])/d_theta, (a - theta.g[k,indE])/d_theta)
      del1sE[is.na(del1sE)] <- -Inf
      if(length(del1sE[del1sE < 0]) == 0){ del1s <- -Inf
      }else{del1s <- max(del1sE[del1sE < 0])}
      #if(del1s >= 0) stop("Non-neg stepsize E -> L/R \n")
    }
    ### exception handling transition \ell_1 path -> \ell_2 path
    
    ### a non-elbow point hits the elbow
    num = (theta0 + drop(x[-indE, indV, drop = FALSE] %*% lambeta)) - lambda2[k] * y[-indE]
    denum = -(d_theta0 + x[-indE, indV, drop = FALSE] %*% d_lambeta) + y[-indE]
    del2sLR = rep(NA, n)
    del2sLR[-indE] = num / denum
    del2sLR[is.na(del2sLR)] <- -Inf
    del2sLR[del2sLR > 0  & del2sLR < eps1] <- 0 
    del2sLRp = del2sLR[del2sLR < -eps1]
    if(k == 1){ del2sLRp = del2sLR[del2sLR <= 0]}
    if(length(del2sLRp) == 0){ del2s <- -Inf
    } else{ del2s <- max(del2sLRp) }
    
    #if(del2s > 0) stop("Non-neg stepsize transition L/R -> E \n") 
    
    ### an active variable becomes inactive
    del3sV <- -(lambeta / d_lambeta)
    del3sV[is.na(del3sV)] <- -Inf
    if(length(del3sV[del3sV < -eps1]) == 0){ del3s <- -Inf
    } else {del3s <- max(del3sV[del3sV < -eps1])}
    
    #if(del3s >= 0) stop("Non-neg stepsize transition V -> V^c \n") 
    
    ### an inactive variable joins the active set
    if(m - length(indV) > 0){
      corrVc <-  drop(t(x[, -indV, drop = FALSE]) %*% theta.g[k,]  - rep(t(rep(lambda2[k], length(indV))) %*% beta[k, indV], m - length(indV)))
      d_corr <- drop(t(x[indE,-indV, drop = FALSE]) %*% d_theta) - c(sum(d_lambeta), rep(0, m - length(indV) - 1))
      del4sV <- cbind((-lambda1 - corrVc)/d_corr, (lambda1 - corrVc)/d_corr)
      ###
      del4sV[del4sV >= -eps1] <- -Inf
      del4sV[is.nan(del4sV)] <- -Inf
      ###
      del4s <- max(apply(del4sV, 1, max))
    } else {del4s <- -Inf}
    if(del4s > 0) {
      #cat("Non-pos stepsize transition V^c -> V \n")
      break
    }
    
    ### v. Lambda2 drops to zero.
    del5s <- -lambda2[k]
    
    #### find out which event occurs first.
    deltas <- c(del1s, del2s, del3s, del4s, del5s)
    delta <- max(deltas)
    event <- which(deltas == delta)
    #if(length(event) > 1) cat("\t mult. events \n")
    #cat("\t Event", event, "takes place and delta equals", delta, "\n")
    ###
    
    ### update relevant quantities, using old status sets.
    theta.g[k + 1, ] = theta.g[k,]
    theta.g[k + 1, indE] = theta.g[k, indE] + delta * d_theta
    
    ### point should not go beyond the border of theta
    if(any(theta.g[k + 1, indE] < -(1 - a))){
      theta.g[k + 1, indE][theta.g[k + 1, indE] < -(1 - a)] = -(1 - a) #if a point moves from E to L
    }else if(any(theta.g[k + 1, indE] > a)){
      theta.g[k + 1, indE][theta.g[k + 1, indE] > a] = a #if a point moves from E to R
    }
    
    ### update coefficients, lambda2 & residuals
    beta0[k + 1] <- (theta0 + delta * d_theta0)/(lambda2[k] + delta)  
    beta[k + 1, indV] <- (lambeta + delta * d_lambeta)/(lambda2[k] + delta)
    beta[indV][abs(beta[k + 1, indV]) < eps1] <- 0     
    
    lambda2[k + 1] <- lambda2[k] + delta
    #cat("\t Lambda2 reduces to", lambda2[k + 1], "\n")
    
    res[-indE] <- y[-indE]  - drop(x[-indE, , drop = FALSE] %*% beta[k + 1, ] + beta0[k + 1])
    
    resAll <- y  - drop(x %*% beta[k + 1, ] + beta0[k + 1])
    checkfct[k + 1] = sum(theta.g[k + 1,] * resAll)
    trHat = length(indE)
    Cgacv[k + 1] = checkfct[k + 1] / (n - trHat)
    Csic[k + 1] = log(checkfct[k + 1]/n) + (log(n)/(2*n))*trHat
    Cgacv[which(is.na(Cgacv))] = Inf #neu 8.5.
    
    if(any(is.nan(res)))
      break
    if(any(res[indL] > eps2) & lambda2[k + 1] > eps2){ 
      #cat("Positive residual in L \n")
      break
    }
    if(any(res[-indL] < -eps2) & lambda2[k + 1] > eps2){
      #cat("Negative residual in R \n")
      break}
    
    
    # if(any(res[indL] > eps2) & lambda2[k + 1] > eps2) {
    #   cat("Positive residual in L \n")
    #   break }
    # if(any(res[-indL] < -eps2) & lambda2[k + 1] > eps2) {
    #   cat("Negative residual in R \n")
    #   break }
    
    ### Update of the different status sets
    if(!is.na(match(1, event))){
      indoutEPnt <- which(del1sE == max(del1sE[del1sE < 0]), arr.ind = TRUE)
      if(max(del1sE[del1sE < 0]) == -Inf){
        indoutEPnt <- 1:length(indE)
      }
      #if(length(indoutEPnt) > 1) cat("\t multiple points move out of E\n")
      for(j in 1:length(indoutEPnt)){
        if (abs(theta.g[k + 1, indE[indoutEPnt[j]]] - a) < eps1){ #point moves out of the elbow to R
          indR <- sort(c(indR, indE[indoutEPnt[j]]))
          theta.g[k + 1, indE[indoutEPnt[j]]] <- a
        }
        if(del1s == 0 & d_theta[indoutEPnt[j]] < 0 ){
          indR <- sort(c(indR, indE[indoutEPnt[j]]))
          theta.g[k + 1, indE[indoutEPnt[j]]] <- a
        }
        if (abs(theta.g[k + 1, indE[indoutEPnt[j]]] +  (1 - a)) < eps1){ #point moves out of the elbow to L
          indL <- sort(c(indL, indE[indoutEPnt[j]]))
          theta.g[k + 1, indE[indoutEPnt[j]]] <- -(1 - a)
        }
        if(del1s == 0 & d_theta[indoutEPnt[j]] > 0 ){
          indL <- sort(c(indL, indE[indoutEPnt[j]]))
          theta.g[k + 1, indE[indoutEPnt[j]]] <- -(1 - a)
        }
      }
      indE <- indE[-indoutEPnt]
    }
    
    if(!is.na(match(2, event))){
      indnewEPnt <- which(del2sLR == del2s, arr.ind = TRUE)
      indE <- c(indE, indnewEPnt)
      for(j in 1:length(indnewEPnt)){
        if(sum(!is.na(match(indL, indnewEPnt[j]))) > 0){
          indL <- indL[-which(indL == indnewEPnt[j])]
        }  
        if(sum(!is.na(match(indR, indnewEPnt[j]))) > 0){
          indR <- indR[-which(indR == indnewEPnt[j])]
        }
      } 
    }
    
    if(!is.na(match(3, event))){
      removedVar <- which(del3sV == del3s, arr.ind = TRUE)[1]
      indV <- indV[-removedVar]
      for(j in 1:length(removedVar)){
        Sign[removedVar[j]] <- 0 
      }
    }
    
    if(!is.na(match(4, event))){
      indnewVar <- which(abs(del4sV - del4s) < eps2, arr.ind = TRUE)
      newVar <- which(dimnames(x)[[2]] == dimnames(indnewVar)[[1]])
      #cat("\t point", newVar,"joins the active set\n")
      indV <- c(indV, newVar)
      for(j in 1:length(newVar)){ 
        if(del4s == del4sV[indnewVar[j],1]){
          Sign[newVar[j]] <- -1
        }     
        if(del4s == del4sV[indnewVar[j],2]){
          Sign[newVar[j]] <- 1
        }
      } 
    }
    
    if(event == 5){ lambda2[k + 1] <- 0
    break}
    
    temp <- res
    temp[abs(temp) < eps1] = 0
    ObjVal[k + 1] = sum(a * temp[temp > 0]) - (1 - a) * sum(temp[temp < 0]) 
    ###
    
    if (allPredIncluded == TRUE){
      #cat("All predictors are now included in the model!\n")
      break
    }
    
    if(length(indE) == length(y)){
      #cat("All points have reached the ellbow \n")
      break 
    }  
    
    if (length(indV) == m){
      allPredIncluded <- TRUE
    }
    
    if(m - length(indV) > 0){
      corrV <- drop(t(x[, indV, drop = FALSE]) %*% theta.g[k + 1, ]) - lambda2[k + 1] %*% beta[k + 1, indV]
      corrVc <-  drop(t(x[, -indV, drop = FALSE]) %*% theta.g[k + 1,]  - rep(t(rep(lambda2[k + 1], length(indV))) %*% beta[k + 1, indV], m - length(indV)))
    }
    
    ### Calculate criteria SIC and GACV
    
    
    E.list[[k + 1]] = indE
    L.list[[k + 1]] = indL
    R.list[[k + 1]] = indR
    V.list[[k + 1]] = indV
    Sign.list[[k + 1]] = Sign
  }
  
  
  ########## Loop Ends ###########
  
  # return(list(Beta0 = beta0, Beta = beta, Lambda2 = lambda2, ObjValue = ObjVal, Step = k, Elbow.List = E.list,
  #             L.list = L.list, R.list = R.list, V.list = V.list, CGACV = Cgacv[0:k], CSIC = Csic[0:k], Theta = theta.g))
  return(list(Beta0 = beta0, Beta = beta, Lambda2 = lambda2, ObjValue = ObjVal, Step = k, Elbow.List = E.list,
              L.list = L.list, R.list = R.list, V.list = V.list, CGACV = Cgacv[0:k+1], CSIC = Csic[0:k+1], Theta = theta.g))
  
}

