

rm(list=ls())
 
# 1) 3 random cards, all need to be guessed correctly from a deck of 52
# 2) you are given a card and then 2 random cards need to both be guessed correctly
# 3) swap given card for a random, all need to be guessed correctly from a deck of 51

tmpfun <- function(card1=NULL, n=13) {
  
  
  pack=rep(1:n, times=4)
  card1 <- abs(card1)
  x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
  pack2 <- pack[-x]                             # remove from pack 1 card of value that is given to us  
  hand <- c(sample(pack2, 3, replace=F) )       # select 3 random cards, deck must be missing given card (N=19)
  
  # --------------------------------
  # Here is the engine!
  # 2--------------------------------Guess card2 
  
  # if the first card is <8 we should guess higher, record if we are correct or not.
  if (hand[1] <  ceiling(n/2) ) { 
    H1 <- hand[2] > hand[1]   
    L1=NA
  }  else { 
    # if the first card is not <9 we should guess lower, record if TRUE (or FALSE if we are wrong).
    L1 <- hand[2] < hand[1]   
    H1=NA
  }
  # --------------------------------
  
  # 3--------------------------------after guessing card 2 value relative to card1 correctly we proceed
  
  if (isTRUE(H1)|isTRUE(L1)) { 
    # if the 2nd card is <9 we would guess higher, record if we are correct of not.
    
    if (hand[2] < ceiling(n/2) ) { 
      H2 <- hand[3] > hand[2]  
      L2=NA
    }  else { 
      # if the 2nd card is not <9 we would guess lower, record if TRUE (or FALSE if we are wrong).
      L2 <- hand[3] < hand[2]  
      H2=NA
    } 
    
  } else {
    
    H2=NA # need this so we always populate H2 L2
    L2=NA
  }
  
  # 4--------------------------------collect information
  
  ret = list()  # collect information after a loop is executed
  ret$H1  = H1 # H1 prop of 1st cards that are < 9 and card 2 is of greater value
  ret$L1  = L1 # L1 prop of 1st cards that are > 9 and card 2 is of lesser value
  ret$H2  = H2 # H2 after correctly guessing 2nd card, prop of 2nd cards that are < 9 and card 3 is of greater value than card 2
  ret$L2  = L2 # L2 after correctly guessing 2nd card, prop of 2nd cards that are > 9 and card 3 is of lesser value than card 2
  
  return(list(ret) )  # output the information
  
}

# 5 ---------------------------run the simulations and manage the output
# three choices enter "3 random" and 3 cards are chosen at random to play the game . expect 50%
# enter a positive integer between 1 and 14. That is the first card you cannot change it . Q1 use 11 for a jack expect 48%
# enter a negative integer between 1 and 14. That is the first card you always change. Enter -11 for Question 2.  %50?
# enter 0. A random first card always change.50%?
# enter 99. play the game optimally changing on cards 5:11 ~ 55%
sims <- 1000000
set.seed(124142)
 
result <- replicate(sims, tmpfun(card1=-7, n=13))


# managing the results...this seems like it could be shortened
x <- unlist(result)
xx <-  as.data.frame(cbind(result=data.frame(x), test =names(x)))
xx$id <- ave(xx$test ,xx$test ,  FUN = seq_along)   
require(tidyverse)
z <- xx %>% spread(test, x)
apply(z[,2:5],2,mean, na.rm=T)  

zz <- z[,2:5]
zz <- zz*1  
zz$card2 <- rowSums(zz[,c("H1", "L1")], na.rm=TRUE)  # any win here is counted
zz$card3 <- rowSums(zz[,c("H2", "L2")], na.rm=TRUE)  # any win here is counted
zz$card23 <- rowSums(zz[,c("card2", "card3")], na.rm=TRUE) # both have to be guessed correctly to win the game
y <-  zz$card23
length(y[y==2])/sims   # proportion of times we win 
