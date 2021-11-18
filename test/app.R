---
title: "Play Your Cards Right Probability"
author: Eamonn O'Brien
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: scroll
    social: menu
    source_code: embed
runtime: shiny
---

```{r global, include=FALSE}

library(Hmisc)
library(plotly)
library(ggplot2)

```


Column {.sidebar}
-----------------------------------------------------------------------

```{r}

 br()
numericInput(inputId=("Decksize"),  
             label = c("No of cards in each suit in deck"), 
             value = 13, min=3,max=101, step=2)

               numericInput(inputId=("swap"),   
                            label = c("Reject this for another random card"), 
                            value = 10, min=1,max=101, step=1)
                
    

```

### Info

*Purpose*:   
In the TV program Play Your Cards Right players are dealt one card face up and several cards face down from a standard pack of 52 cards. They then have to guess whether the next card
is higher or lower than the overturned card. Get it right and
the game continues. Get it wrong, or turn over the same card of a
different suit, and the game ends.

Here we limit the game to three cards. We examine the scenario in which the player always keeps the first card and the scenario in which the player always replaces the first card dealt. Instead of using a standard playing card deck we use the numbers 1 to 13 and have 4 copies to mimic a deck of cards.

The question is, what is the probability that both your guesses are correct? 

For example, a Jack is represented by card value 10. There are then 9x4=36 cards lower than the
Jack, there are 51 cards unseen, so the probability that the first mystery card is lower than a Jack can be calculated as 36/51 ≈ 70.6%.


*Method*:  
An explanation for the scenario in which a card is dealt and not switching is explained [here](https://www.google.com "Google's Homepage")

We expand upon this for our question, and evaluate for all possible scenarios for the first card (remembering we have thrown a particular card away).    

            
               



Column 
-----------------------------------------------------------------------
### Should you swap the swap the first card your are dealt?

*Purpose*:   
In the TV program Play Your Cards Right players are dealt one card face up and several cards face down from a standard pack of 52 cards. They then have to guess whether the next card
is higher or lower than the overturned card. Get it right and
the game continues. Get it wrong, or turn over the same card of a
different suit, and the game ends.

Here we limit the game to three cards and always replace the first card we are dealt. 
Instead of using a standard playing card deck we use the numbers 1 to 13 and have 4 copies to mimic a deck of cards.

We are dealt a card of your choosing [2nd box on left] and always discard it to replace it with a random card and two mystery cards. The question is what is the probability that both your guesses are correct. 

For example, a Jack is represented by card value 10. There are then 9x4=36 cards lower than the
Jack, there are 51 cards unseen, so the probability that the first mystery card is lower than a Jack can be calculated as 36/51 ≈ 70.6%.


*Method*:  
An explanation for the scenario in which a card is dealt and not switching is explained [here](https://www.significancemagazine.com/culture/706-probability-puzzle-higher-or-lower "").  We expand upon this for our question, and evaluate for all possible scenarios for the first card (remembering we have thrown a particular card away).    

           

```{r}


#################------------------------------------------

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


#############################----------------------------




 
    
    pycr <- function( pack.size=13, card1=8 ) {
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
  
  
  
  
  ################start middle card
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
  
  ### now we assess the impact of the first card being >=8
  
  
  step <-  (section1+2):pack.size  #%%%%%%%%%%%%%%%%%%%%%%%%%%%change
  
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
  
  
  
  #print(nu)
  print(MASS::fractions(numerator/denominator))
}
    

renderPrint({   
    
 
    "This is the probabilty of winning when we keep the card of the value entered in input box 2 and play optimially"
    
    
})

renderPrint({
    
 
    pycr1( pack.size=input$Decksize, card1=input$swap) 
    
    
})
 

    
renderPrint({
    
 
    "This is the probabilty of winning when we discard the card of value in entered input box 2, replace it with a random card and play optimially"
    
    
})
     
    
    

renderPrint({
    
 
    pycr( pack.size=input$Decksize, card1=input$swap) 
    
    
})
 


```

### Info

*Purpose*:   
In the TV program Play Your Cards Right players are dealt one card face up and several cards face down from a standard pack of 52 cards. They then have to guess whether the next card
is higher or lower than the overturned card. Get it right and
the game continues. Get it wrong, or turn over the same card of a
different suit, and the game ends.

Here we limit the game to three cards and always replace the first card we are dealt. 
Instead of using a standard playing card deck we use the numbers 1 to 13 and have 4 copies to mimic a deck of cards.

We are dealt a card of your choosing [2nd box on left] and always discard it to replace it with a random card and two mystery cards. The question is what is the probability that both your guesses are correct. 

For example, a Jack is represented by card value 10. There are then 9x4=36 cards lower than the
Jack, there are 51 cards unseen, so the probability that the first mystery card is lower than a Jack can be calculated as 36/51 ≈ 70.6%.


*Method*:  
An explanation for the scenario in which a card is dealt and not switching is explained here. We expand upon this for our question, and evaluate for all possible scenarios for the first card (remembering we have thrown a particular card away).    


*Notes*:  