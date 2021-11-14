
n <- 13

pycr <- function( pack.size=13, card1=11 ) {
  ###########################################analytical removing JACK
  
  # pack.size<-n<-13
  # card1=10
  # 
  
  pack <- rep(1:pack.size, times=4)                    # regula tpack of cards aces high
  x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
  pack2 <- pack[-x]                             # remove card
  
  
  # rv <- 2:14
  # sapply(rv, function(f) sum(pack3<f))
  
  # rows are the 1st card, we look at all 2:14 as it is randomly chosen
  # columns are the counts , suits dont not matter
  r <- matrix(NA, pack.size,pack.size) # capture the  P(guessing correctly)
  
  section1 <- floor(pack.size/2)
  step <- 1:section1  # go through what happens if the forst card is 2:7
  
  for (i in step) {                            # lok at 2,3,4...7
    
    x <- which(pack2==i)[1]                    # remove first card
    pack3 <- pack2[-x]  
    
    # ---------------------------------
    # if we have a 2 we should guess higher
    # loop starts at i so we go along the ith row indenting 1 each ime
    for (j in i:pack.size) {             # assess impact of guess, 14 here as we start at 2
      
      # if we have a 2 for example we should guess the next car is higher 
      # fo through each card and assess and count the opportunities
      
      A <-  sum(pack3<j) #  
      B <-  sum(pack3>j) #
     # print(A);print(B)
      if( i != j) {
        
        # assess if we guess higher lower or 50:50 
        
        x=max(A,B)
      #  print(x)
        
        r[i,j]<-x    # add to matrix
        
      }
       
    }
    
    #----------------------------
    
  }
  
  ### now we assess the impact of the first card being >=8
 
  
  step <-  (section1+1):pack.size
  
  for (i in step) {   # first card
    
    x <- which(pack2==i)[1]                    
    pack3 <- pack2[-x]  
    
    
    # ---------------------------------
    for (j in 1:i) {                    # 2 is the 1st column, i varies incraesing by 1 each time
      
      A <-  sum(pack3<j) #
      B <-  sum(pack3>j) #
      
      if( i != j) {
        
        x=max(A,B)
        r[i,j]<-x #
        
      }
      
      if (j==max(step)) {print(r)}
    }
    
    
    #----------------------------
    
  }
  #####################
  
  
  # Now do the calculations
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  f <- matrix(4/((pack.size*4)-2), pack.size,pack.size)
  f[,card1] <- (3/((pack.size*4)-2))
  g <- r*f
  
  # print(mean(rowSums(g, na.rm=TRUE)/((pack.size*4)-3)))
  # account for fact the prob of observing first card is not the same for all cards
  w <- table(pack2)/length(pack2) # weights
  
  denom <- (pack.size*4)-3
  numer <- rowSums(g, na.rm=TRUE)
  A <- numer/denom
  B <- A*w   # adjust using weight, the probability of the firs card being selected
  print( sum(B), digits=10)
  
  ##pront as fraction 
  
  
  # work on numerators
  A <- 4*rowSums(r[,-card1], na.rm=TRUE) 
  B <- 3*r[,card1]
  num <-  rowSums(cbind(A,B),na.rm=TRUE)
  
  den1 = (pack.size*4)-2 
  den2 = (pack.size*4)-3
  den <- den1*den2 # rep(den1*den2, pack.size)
  
  w1 <- as.vector(table(pack2))
  w2 <- length(pack2) #rep(length(pack2), pack.size)# weights
  
  # here is the answer as a fraction
  numerator <- sum(num*w1)
  denominator <- den*w2
  
  print(MASS::fractions(numerator/denominator))
}


# throw away card first card and replace randomly, the analytical prob of winning 
for(i in 1:n){
  cat(paste0("We remove and replace card ",print(i),"\n"))
  pycr(pack.size=n,card1=i)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pycr(pack.size=10, card1=4)
pycr(pack.size=5, card1=3)

pycr(pack.size=13, card1=6)
pycr(pack.size=13, card1=7)
pycr(pack.size=13, card1=8)
pycr(pack.size=13, card1=9)
pycr(pack.size=13, card1=10)
pycr(pack.size=13, card1=11)
pycr(pack.size=13, card1=12)
pycr(pack.size=13, card1=13)

# 
# # work on numerators
# A <- 4*rowSums(r[,-card1], na.rm=TRUE)
# B <- 3*r[,card1]
# num <-  rowSums(cbind(A,B),na.rm=TRUE)
# 
# den1 = (n*4)-2 
# den2 = (n*4)-3
# den <- den1*den2 # rep(den1*den2, pack.size)
# 
# w1 <- as.vector(table(pack2))
# w2 <- length(pack2) #rep(length(pack2), pack.size)# weights
#  
# # here is the answer as a fraction
# numerator <- sum(num*w1)
# denominator <- den*w2
#  
# print(MASS::fractions(numerator/denominator))



