
 
 # strategy if card less than 9 guess higher.
 # We prefer extreme card to start
 # if first card 5	6	7	8	9	10 11 ask to replace as there are more extreme cards than non extreme.
 # eg if your card is a jack (11), there are 1,2,3 and 12,13,14 which are better, so 6 better and only 5 cards same or less extreme 
 # 6,7,8,9,10 
 
  
 # the code caters for card of value 8. If an 8 turns up, this is <9 so we guess higher all the time.
 # for an 8 we should guess randomly as there are the same number of higher valued cards as lower valued cards
 # so always guessing higher has the same effect, expecting to be correct 50% of the time.
   
 rm(list=ls())
 
 pack <- rep(2:14, times=4)  # a proper deck of cards
 
 # 1) 3 random cards, all need to be guessed correctly from a deck of 52
 # 2) you are given a card and then 2 random cards need to both be guessed correctly
 # 3) swap given card for a random, all need to be guessed correctly from a deck of 51
 
 tmpfun <- function(card1=NULL) {
   
   # 1--------------------------------select the first card or randomly select a first card 
   if (card1 %in% "3 random") {
     
      hand <- sample(pack, 3, replace=F)             # select 3 random cards
     
    }  else  if (card1 %in% 2:14) {                 # the game, not allowing a change of first card. Q1 that card is a jack
     
      x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
      pack2 <- pack[-x]                             # remove from pack 1 card of value that is given to us  
      hand <- c(card1,sample(pack2, 2, replace=F) ) # select given card and 2 random cards, deck must be missing given card 
      
    }  else  if (card1 %in% -2:-14) {               # the game, but this time always changing the first card
      
      card1 <- abs(card1)
      x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
      pack2 <- pack[-x]                             # remove from pack 1 card of value that is given to us  
      hand <- c(sample(pack2, 3, replace=F) )       # select 3 random cards, deck must be missing given card (N=51)
      
    } else  if (card1 %in% 0) {                     # random version of above 3 random cards from a deck of 51
      
      card1 <- c(sample(pack, 1, replace=F) )       # random remove a  card
      x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
      pack2 <- pack[-x]                             # remove from pack 1 card of value that is given to us  
      hand <- c(sample(pack2, 3, replace=F) )       # select 3 random cards, deck must be missing given card (N=51)
   
    } else  if (card1 %in% 99) {{                    # the game but change card optimally 
      
      card1 <- c(sample(pack, 1, replace=F) )       # random select a  card
    
    }
       if (card1 %in% c(5:11)) {                    # if it better to change first card (cards valued 5:11), always change, if not keep the card
        x <- which(pack==card1)[1]                  # find the cars
        pack2 <- pack[-x]                           # remove the card  
        cardx <- sample(pack2, 1, replace=F)        # pick another
        x <- which(pack2==cardx)[1]                 # find and remove this replacement card from pack   
        pack3 <- pack2[-x]                           
        hand <- c(cardx,sample(pack3, 2, replace=F) ) # sample 2 more cards from pack
        
       }  else {       
         
       x <- which(pack==card1)[1]                    # identify card location in pack, the 1st 
       pack2 <- pack[-x]                             # remove from pack 1 card of value that is given to us  
       hand <- c(card1,sample(pack2, 2, replace=F) )
      
     
    } }
   
   # --------------------------------
   # Here is the engine!
   # 2--------------------------------Guess card2 
   
    
    
    # if the first card is <8 we should guess higher, record if we are correct or not.
   if (hand[1] < 8 ) { 
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
      
     if (hand[2] < 8 ) { 
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
   result <- replicate(sims, tmpfun(card1="3 random")) # 
   result <- replicate(sims, tmpfun(card1= 11)) # 
   result <- replicate(sims, tmpfun(card1=-11))
   result <- replicate(sims, tmpfun(card1= 0)) 
   result <- replicate(sims, tmpfun(card1= 99))
   
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
   
   #-----------------------------------------------------------------------------------------------------------
   
   
   ## run the simulations in a loop
   simul <- function(xx, sims=100000) {
      
      result <- replicate(sims, tmpfun(card1=xx))
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
      print(length(y[y==2])/sims)   # proportion of times we win 
      
      
   }
   
   for(i in -2:-14) {
      simul(i, sims=100000) 
   }
   
   
     
   
   
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # ANALYTICAL 
      
   # starting with 2
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28,32,36,40,44)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 #
 
   # starting with 3
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28,32,36,40)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   # starting with 4
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28,32,36)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   
   
   # starting with a 5, guess higher
   LJ <- 4/51 # how many chances of choosing a x value card
   # look at each values above 5 , calculate probability of getting 6,7,8,9,10,11,12,13,14
   p1 <- c(47,43,39,35,31,27) # prob of < 9, <10...<14
   p2 <- c(24,28,32)          # prob of getting >6, >7, >8
   
   LJ * sum(c(p1,p2)) / 50   #306
   
   
   # starting with 6
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   # starting with 7
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   
   # starting with 8
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(0)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   # starting with 9
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   # starting with 10
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   
   # lower than jack 11
   LJ <- 4/51
   p1 <- c(47,43,39,35,31,27) # prob of choosing 2 and next card > 2; same for 3,4,5,6,7
   p2 <- c(24,28,32)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   
   
   # starting with 12 queen
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28,32,36)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   
   # starting with 13 king
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28,32,36,40)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
  
   # starting with 14 Ace
   LJ <- 4/51
   # look at all values above 3 and then note the prob changes at card value 8
   p1 <- c(47,43,39,35,31,27) # prob of < 9
   p2 <- c(24,28,32,36,40,44)          # when 8 is observed prob next card is < 8 same for 9 10
   
   LJ * sum(c(p1,p2)) / 50 
   
   
   
   
   
   
   
   
  
  
  
  
  
  
  