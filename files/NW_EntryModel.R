# Nelson Winter Entry Model
# The model is based on the NW model discussed in Chapte 12 of 
# Nelson, R.R., and Winter, S.G. (1982), 
# "An Evolutionary Theory of Economic Change",
# Belknap Press, Cambridge, Mass. and London.
#
# Version 1.0 by
# Erol Taymaz
# Department of Economics
# Middle East Technical University
# Ankara Turkey
# www.metu.edu.tr/~etaymaz/nwm
# 18 September 2011

# R OPTIONS
# Enter recover option so that debugging will be easier
options(error=recover)
# Convert warnings to errors
options(warn=2)



# Run Simulations - nIter: number of iterations, nFirm: number of firms, setSeed: set the initial seed to 123 (TRUE) or not (FALSE)
run <- function(nIter=100, nFirm=8, setSeed=FALSE) {
  
  # Set parameters
  param <<- setPara(setSeed)

  # Attach param list so that each paremeter will be used by its name
  attach(param)
  
  # Initialize firms
  firms <<- setFirms(nFirm)
  
  # Initialize markets
  market <<- setMarket(nFirm)
  
  # Data collection variables
  outTech <<- list(c(max(firms$A),firms$A))
  outMS   <<- list(firms$q/sum(firms$q))
  outID   <<- list(c(1:nFirm))
  
  # Simulate the model
  for (time in c(2:nIter)) iterate(time)
  
  # Detach parameter list
  detach(param)
  
  # Re-make outTech and outMS
  outTech <<- remakeTech()
  outMS   <<- remakeMS()
  
  # Plot results
  pMarket()
  
  # Plot market shares and technological level variables by using
  # functions pMS() and pTech()
  }

# Parameter setting function
setPara <- function(setSeed) {

  # Set the seed for random number generator if setSeed parameter is TRUE
  # Default is FALSE
  if (setSeed==TRUE) set.seed(123)
  parameters <- list(
    rin   = 0.0205,     # Innovation expenditures, rin
    rim   = 0.00102,	  # Imitation expenditures, rim
    cc    = 0.160,		  # Cost of capital, c
    an    = 0.1250,		  # Innovation parameter, an
    am    = 1.250,	   	# Imitation parameter, am
    dep   = 0.030,		  # Depreciation rate
    dL    = 67.000,	   	# Demand level parameter
    dG    = 0.010,		  # Demand shift parameter
    tL    = 0.160,		  # Level
    tG    = 0.010,		  # Rate of change
    tSd   = 0.050,		  # Std dev
    Bank  = 1,		      # Bank
    K     = 390.8, 	    # Initial total capital stock
    nMax  = 5,          # Max number of new firms
    nExo  = 0.1,        # Exogenous entry probability
    nPro  = 2.5,        # Profit elasticity of entry rate
    newS  = 0.75,       # Average entry size
    newSd = 0.75,       # Average std dev of entry size
    newA  = 0.75,       # Average tech level of new firms
    newAd = 2,          # Ave std dev of tech level of new firms
    minEntK = 1,        # Min K stock of new firms
    minEntA = 0.1,      # Min tech level of new firms
    maxNegProf = 5,     # Max periods of negative profit before entry
    minNFirm   = 3
    )
    return(parameters)
  }
 
# Firm initialization function
setFirms <- function(nFirm) {

  # Set data frame for firms
  firms <- data.frame(matrix(0, nrow=nFirm, ncol=7))
  names(firms) <- c("id", "k", "A", "q", "profit", "I", "negP")

    firms$id       <- c(1:nFirm)        # Firms' id number
    firms$k        <- K/nFirm           # Capital stock
    firms$A        <- exp(tL)           # Technology
    firms$q        <- firms$k*firms$A   # Output
    price          <- dL/sum(firms$q)   # Product price
    firms$profit   <- (price * firms$A) - cc - rin -rim
    firms$negP     <- rep(0, nFirm)     # Performance counter
    firms$I        <- invest(price, firms$q, firms$A, firms$profit)
  return(firms)
  }

# Market initializaton function
setMarket <- function(nFirm) {
  # Set data frame for the market
  market <- data.frame(matrix(0, nrow=0, ncol=8))
  names(market) <- c("Q", "P", "HI", "tF", "A", "Profit", "K", "invSh")

  market[1,"Q"]    <- sum(firms$q)
  market[1,"P"]    <- dL/sum(firms$q)
  market[1,"HI"]   <- 1/sum((firms$q/sum(firms$q))^2)
  market[1,"tF"]   <- max(exp(rnorm(1,tL,tSd)),market$tMax[1])
  market[1,"A"] <- sum(firms$k*firms$A)/sum(firms$k)
  market[1,"Profit"] <- sum(firms$k*firms$profit)/sum(firms$k)
  market[1,"K"]    <- sum(firms$k)
  market[1,"invSh"]    <- sum(firms$I)/sum(firms$k)
  return(market)
  }

# Investment function
invest <- function(P, q, A, profit) {
  ms <- q/sum(q)
  cash <- ifelse(profit<0, profit, profit*(1 + Bank))
  desInv <- 1 + dep - (2-ms)/((P*A/cc)*(2-2*ms))
  return((q/A)*pmax(0,pmin(cash,desInv)))
  }

# Innovation function
  innDraw <- function(K) {
  return(rin*an*K > runif(length(K)))
  } 

# Imitation function
  immDraw <- function(K) {
  return(rim*am*K > runif(length(K)))
  } 

# New firm entry function
enterFirms <- function() {
  nNew <-sum(runif(nMax) < (nExo + nPro*mean(firms$profit)))
  if (nNew>0) {
    aveK <- mean(log(firms$k))
    sdK  <- sd(log(firms$k))
    aveA <- mean(log(firms$A))
    sdA  <- sd(log(firms$A))
    newId<- max(firms$id)+c(1:nNew)
    newk <- max(minEntK, exp(rnorm(nNew, newS*aveK, newSd*sdK)))
    newA <- max(minEntA, exp(rnorm(nNew, newA*aveA, newAd*sdA)))
    
    newq <- newProfit <- newI <- newNP <- rep(0, nNew)
    newFirms <- data.frame(newId, newk, newA, newq, newProfit, newI, newNP)
    names(newFirms) <- c("id", "k", "A", "q", "profit", "I", "negP")
    firms <<- rbind(firms, newFirms)
    }
  }

# Exit function
exitFirms <- function(){
    survive <- maxNegProf > firms$negP
    if (sum(survive) > minNFirm) firms <<- firms[survive,]
  }
  
# Iteration function
iterate <- function(time) {

  id     <- firms$id                  
  innDr  <- innDraw(firms$k)
  immDr  <- immDraw(firms$k)
  k      <- pmax(0,firms$I+((1-dep)*firms$k))
   
  tMax  <- max(firms$A)
  tF    <- max(exp(rnorm(1,tL+((time-1)*tG),tSd)), market$tF[time-1], tMax)

  A     <- pmax(firms$A, 
                       innDr*(firms$A + runif(length(firms$A))*(tF-firms$A)),
                       immDr*(firms$A + runif(length(firms$A))*(tMax-firms$A)))
  q     <- k*A
  Q     <- sum(q) 
  P     <- (dL*(1+dG)^(time-1))/Q
  HI    <- 1/sum((q/sum(q))^2)

  profit<- (P * A) - cc - rin - rim
  I     <- invest(P, q, A, profit)
  negP  <- pmax(0, firms$negP + ifelse(profit>0, -1, 1))
  
  avePr <- sum(k*profit)/sum(k)
  aveA  <- sum(k*A)/sum(k)
  sumK  <- sum(k)
  aveISh<- sum(I)/sum(k)
  
  # Change/append global variables
  firms         <<- data.frame(id, k, A, q, profit, I, negP)
  market[time,] <<- c(Q, P, HI, tF, aveA, avePr, sumK, aveISh)
  
  # Append data collection variables
  outTech[[time]] <<- c(tF, A)
  outMS[[time]]   <<- q/Q
  outID[[time]]   <<- id
  
  # Exit firms
  exitFirms()
  
  # Enter new firms
  enterFirms()
  
  # Print iteration number
  # if ((time%%10)==0) cat("Iteration : ", time, "\n")
  }

# Graphics functions
pMarket <- function() {
  # Create a layout for plots
  # Four charts will be plotted on the same page
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(market$P, type="l", ylim=c(0, max(market$P)), main="Product price", xlab="Time", ylab="Price")
  plot(market$HI, type="l", ylim=c(0, max(market$HI)),main="Concentration level", xlab="Time", ylab="1/HI")
  plot(log(market$tF), type="l", main="Technology frontier", xlab="Time", ylab="Frontier level (log)")
  plot(log(market$A), type="l", main="Average productivity", xlab="Time", ylab="Average productivity (log)")
  }

pAccum <- function() {
  # Create a layout for plots
  # Four charts will be plotted on the same page
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(ma(growth(market$Q), 5), type="l", main="Output growth rate", xlab="Time", ylab="Growth rate, 5-year MA (%)")
  plot(ma(growth(market$K), 5), type="l", main="Capital stock growth rate", xlab="Time", ylab="Growth rate, 5-year MA (%)")
  plot(ma(100*(market$Profit), 5), type="l", main="Average rofit rate", xlab="Time", ylab="Profit rate (%)")
  plot(ma(100*market$invSh, 5), type="l", main="Average investment rate", xlab="Time", ylab="Investment rate (%)")
  }

pTech <- function() {
  x <- c(1:dim(outTech)[1])
  n <- dim(outTech)[2]
  layout(matrix(c(1), 1, 1))
  plot(x,log(outTech[,1]), type="n", main="Technological level", xlab="Time", ylab="Technological level (log)")
  for (i in c(1:n)) lines(x, log(outTech[,i]), type="l", col=palette()[1+i%%8])
  }

pMS <- function() {
  x <- c(1:dim(outMS)[1])
  n <- dim(outMS)[2]
  layout(matrix(c(1), 1, 1))
  y1 <- floor(100*min(apply(outMS, 1, minNA)))
  y2 <- ceiling(100*max(apply(outMS, 1, maxNA)))
  plot(x,100*outMS[,1], type="n", main="Market shares", ylim=c(y1, y2), xlab="Time", ylab="Market share (%)")
  for (i in c(1:n)) lines(x, 100*outMS[,i], type="l", col=palette()[2+i%%8])
  }

# Additional auxiliary functions
growth <- function(a) {
  n <- length(a)
  g <- 100*log(a[c(2:n)]/a[1:(n-1)])
  return(g)
  }

remakeTech <- function() {
  iter <- length(outID)
  maxID <- 1
  for (i in c(1:iter)) maxID <- max(c(maxID, 1+outID[[i]]))
  Tech <- matrix(NA, nrow=iter, ncol=maxID)
  for (i in c(1:iter)) Tech[i,c(1,1+outID[[i]])] <- outTech[[i]]
  return(Tech)
  }

remakeMS <- function() {
  iter <- length(outID)
  maxID <- 1
  for (i in c(1:iter)) maxID <- max(c(maxID, outID[[i]]))
  MS <- matrix(NA, nrow=iter, ncol=maxID)
  for (i in c(1:iter)) MS[i,outID[[i]]] <- outMS[[i]]
  return(MS)
  }

minNA <- function(x) {
  return(min(x, na.rm=TRUE))
  }
  
maxNA <- function(x) {
  return(max(x, na.rm=TRUE))
  }

ma <- function(x, n) {
  r <- x
  if (n > 0) {
    s <- length(x)
    r <- rep(0, s+1-n)
    for (i in c(1:n)) r <- r + x[c(i:(s+i-n))]
    r <- r/n
    }
  return(r)
  }
