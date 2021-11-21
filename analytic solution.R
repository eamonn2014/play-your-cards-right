

#------------------------------------------------------------------------------------------------------
#-----------------------analytical function delivers prob of guessing 2 mystery cards
#-----------------------The first card that we observe is always swapped
#-----------------------we can pick the deck size and first card value 
#-----------------------inputs pack size and initial card value
#------------------------------------------------------------------------------------------------------



pycr <- function( pack.size=13, card1=8 ) {
  ########################################### analytical  
  
  pack <- rep(1:pack.size, times=4)             # pack of cards aces high
  x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
  pack2 <- pack[-x]                             # remove card
  
  # rows are the 1st card, we look at all 1:n as it is randomly chosen
  # columns are the counts , suits don't not matter
  
  r <- matrix(NA, pack.size,pack.size) # capture the opportunities for guessing correctly
  
  #------------------------------------------------------------------------------------
  # section to work on below median card value
  #------------------------------------------------------------------------------------
  
  section1 <- floor(pack.size/2)
  step <- 1:section1  # go through what happens if the first card is 1: 
  
  for (i in step) {                            # look at 1,2,3,4...median-1
    
    x <- which(pack2==i)[1]                    # remove first card
    pack3 <- pack2[-x]  
    
    # ---------------------------------
    # if we have a 2 we should guess higher
    # loop starts at i so we go along the ith row indenting 1 each time
    for (j in i:pack.size) {             # assess impact of guess, 14 here as we start at 2
      
      # if we have a 2 for example we should guess the next car is higher 
      # fo through each card and assess and count the opportunities
      
      A <-  sum(pack3<j) #  
      B <-  sum(pack3>j) #
      # print(A);print(B)
      if( i != j) {
        
        # assess if we guess higher lower or 50:50 
        x=max(A,B)
        
        r[i,j]<-x    # add to matrix
        
      }
      
    }
    
    #----------------------------
    
  }
  
  
  
  ################work on middle card start middle card
  # 
  # #------------------------------------------
  # # middle card
  mid <-  median(1:pack.size)  # when first card is the middle card
  
  for (i in mid) {   # work only when replaced card is middle card
    
    x <- which(pack2==i)[1]                    # remove first card
    pack3 <- pack2[-x]  
    
    #-------------------------
    if (card1 < i)     {   # now if the initial card that we threw away is less than the middle card
      
      #--------------------------
      for (j in (i+1):pack.size) {  # work on the cards that are higher
        
        A <-  sum(pack3<j) #
        B <-  sum(pack3>j) #
        
        #-------------------
        if( i != j) {
          
          x=max(A,B)
          r[mid,j]<-x
          
        }
        #-------------------
      }
    } else if ( card1 > i)     { # now if the initial card that we threw away is > than the middle card
      
      for (j in (1):(i-1)) {  # work on the cards that are lower
        
        A <-  sum(pack3<j) #
        B <-  sum(pack3>j) #
        
        #-------------------
        if( i != j) {
          
          x=max(A,B)
          r[mid,j]<-x
          
        }
        #-------------------
      }
      #---------------------------
      #------------------------------------------
      
    } else if ( card1 == i)     { # now if the initial card that we threw away is == the middle card
      
      for (j in (1):(i-1)) {  # work on the cards that are lower, it doesnt matter really
        
        A <-  sum(pack3<j) #
        B <-  sum(pack3>j) #
        
        #-------------------
        if( i != j) {
          
          x=max(A,B)
          r[mid,j]<-x
          
        }
        #-------------------
      }
      #---------------------------
    }
    #------------------------------------------------
  }
  #------------------------------------------
  ################end middle card
  
  ### now we assess the impact of the first card being >=median
  
  
  step <-  (section1+2):pack.size  #
  
  for (i in step) {   # first card
    
    x <- which(pack2==i)[1]                    
    pack3 <- pack2[-x]  
    
    
    # ---------------------------------
    for (j in 1:i) {                    # 2 is the 1st column, i varies increasing by 1 each time
      
      A <-  sum(pack3<j) #
      B <-  sum(pack3>j) #
      
      if( i != j) {
        
        x=max(A,B)
        r[i,j]<-x #
        
      }
      
      # if (j==max(step)) {print(r)}
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
  #prob <- B
  ##print as fraction 
  
  
  # work on numerators
  A <- 4*rowSums(r[,-card1], na.rm=TRUE) 
  B <- 3*r[,card1]
  num <-  rowSums(cbind(A,B),na.rm=TRUE)
  
  den1 = (pack.size*4)-2 
  den2 = (pack.size*4)-3
  den <- den1*den2 # rep(den1*den2, pack.size)
  
  w1 <- as.vector(table(pack2))
  w2 <- length(pack2) #rep(length(pack2), pack.size)# weights
  
  nu <- num*w1
  # here is the answer as a fraction
  numerator <- sum(nu)
  denominator <- den*w2
  
  #--------------------------
  # A <- rowSums(r[,-card1], na.rm=TRUE)*4
  # 
  # B <- r[,card1]*3
  # 
  # num <- rowSums(cbind(A,B),na.rm=TRUE)
  # 
  # n <- pack.size*4
  # 
  # den <-   (n-2)*(n-3)
  # 
  # chances <- as.vector(table(pack2))
  # 
  # den2 <- n-1
  # 
  # numerator <- sum(num*chances, na.rm=TRUE)
  # 
  # denominator<- den*den2
  # 
  # res <- numerator/denominator 
  #---------------------------------
  D <- NULL
  A <- NULL
  
  A <- numerator/denominator
  #print(nu)
  D<-  print(MASS::fractions(numerator/denominator))
  
  return(list(D,A))
  
}


# throw away card first card and replace randomly, the analytical prob of winning 
for(i in 1:13){
  cat(paste0("We remove and replace card ",print(i),"\n"))
  pycr(pack.size=13,card1=i)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## odd cards only............
# pycr(pack.size=10, card1=4)
# pycr(pack.size=5, card1=3)

pycr(pack.size=5, card1=1)
pycr(pack.size=5, card1=2)
pycr(pack.size=5, card1=3)
pycr(pack.size=5, card1=4)
pycr(pack.size=5, card1=5)
 

pycr(pack.size=13, card1=1)
pycr(pack.size=13, card1=4)
pycr(pack.size=13, card1=2)
pycr(pack.size=13, card1=3)
pycr(pack.size=13, card1=4)
pycr(pack.size=13, card1=5)
pycr(pack.size=13, card1=6)
pycr(pack.size=13, card1=7) # mid card
pycr(pack.size=13, card1=8)
pycr(pack.size=13, card1=9)
pycr(pack.size=13, card1=10)
pycr(pack.size=13, card1=11)
pycr(pack.size=13, card1=12)
pycr(pack.size=13, card1=13)

pycr(pack.size=5, card1=4)
pycr(pack.size=5, card1=3)

pycr(pack.size=3, card1=1)