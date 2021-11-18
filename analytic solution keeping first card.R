

pycr1 <- function( pack.size=13, card1=10 ) {
  ###########################################analytical keeping delat cardK
  

  pack <- rep(1:pack.size, times=4)             # regular pack of cards aces high
  x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
  pack2 <- pack[-x]     
  
 
  r <- matrix(NA, ncol=pack.size) # capture the  P(guessing correctly)
  
  step <- 1:pack.size  # go through what happens if the forst card is 2:7
  
  for (i in step) {                            # lok at 2,3,4...7
  
      
      A <-  sum(pack2<i) #  
      B <-  sum(pack2>i) #
   
      if( i != card1) {
        
        # assess if we guess higher lower or 50:50 
        
        x=max(A,B)

        r[1,i]<-x    # add to matrix
        
      }
      
      if (card1 < median(1:pack.size)) {
      
       r[1:(card1)] <- NA
      
      }else{
        
      r[(card1):pack.size] <- NA
        
      }
       
       
    }
    
    #----------------------------
   
  
  # Now do the calculations
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  denom <- ((pack.size*4)-2) * ((pack.size*4)-1)
  numer <- rowSums(r, na.rm=TRUE)*4
  A <- numer/denom
  print( sum(A), digits=10)
  
  ##print as fraction 
 
  numerator <- numer
  denominator <- denom
  
  print(MASS::fractions(numerator/denominator))
}


# throw away card first card and replace randomly, the analytical prob of winning 
for(i in 1:13){
  cat(paste0("We keep dealt card ",print(i),"\n"))
  pycr1(pack.size=13,card1=i)
}
