 

pycr <- function(card1=11) {
###########################################analytical removing JACK

pack <- rep(2:14, times=4)                    # regula tpack of cards aces high
x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
pack2 <- pack[-x]                             # remove card


 # rv <- 2:14
 # sapply(rv, function(f) sum(pack3<f))

 # rows are the 1st card, we look at all 2:14 as it is randomly chosen
 # columns are the counts , suits dont not matter
 r <- matrix(NA, 13,13) # capture the  P(guessing correctly)
 

step <- 2:7  # go through what happens if the forst card is 2:7

for (i in step) {                            # lok at 2,3,4...7
  
  x <- which(pack2==i)[1]                    # remove first card
  pack3 <- pack2[-x]  

  # ---------------------------------
  # if we have a 2 we should guess higher
                                # loop starts at i so we go along the ith row indenting 1 each ime
  for (j in i:14) {             # assess impact of guess, 14 here as we start at 2

    # if we have a 2 for example we should guess the next car is higher 
    # fo through each card and assess and count the opportunities
  
    A <-  sum(pack3<j) #  
    B <-  sum(pack3>j) #
    
   if( i != j) {
     
     # assess if we guess higher lower or 50:50 
 
   x=max(A,B)
  
 
          r[i-1,j-1]<-x    # add to matrix
   
   }
   
  }
 
  #----------------------------
 
}

### now we assess the impact of the first card being >=8


step <- 8:14

for (i in step) {   # first card
  
  x <- which(pack2==i)[1]                    
  pack3 <- pack2[-x]  
  
  
  # ---------------------------------
  for (j in 2:i) {                    # 2 is the 1st column, i varies incraesing by 1 each time
    
    A <-  sum(pack3<j) #
    B <-  sum(pack3>j) #
    
    if( i != j) {
 
      x=max(A,B)
      r[i-1,j-1]<-x #
      
    }
    
  }
 
  
  #----------------------------
 
}
#####################

 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f <- matrix(4/50, 13,13)
f[,card1-1] <- 3/50
g <- r*f
print(mean(rowSums(g, na.rm=TRUE)/49))

}

# throw away card first card and replace randomly, the analytical prob of winning 
for(i in 2:14){
  print(i)
pycr(card1=i)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  
# mean(c(0.506719
# , 0.5053375
# , 0.5040816
# , 0.5029513
# , 0.5019466
# , 0.5010675
# ,0.5016013
# , 0.5017896
# , 0.5026688
# , 0.5036735
# ,0.5048038
# ,0.5060597
# , 0.5074411))

