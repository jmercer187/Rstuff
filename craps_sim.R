
RollDie <- function(n) {
  sample(1:6,n, replace = T)
}

ComeOutBet <- function(result, bet, bet_type) {
  
  if ((result == 7 | result == 11) & bet_type == "pass") {
    bet = bet * 2
  }  else if ((result == 2 | result == 3 | result == 12) & bet_type == "pass"){
    bet = 0
  }  else if ((result == 2 | result == 3) & bet_type == "dont pass") {
    bet = bet * 2
  } else if ((result == 7 | result == 11) & bet_type == "dont pass"){
    bet = 0
  } else {
    bet = bet
  }
  
  return(bet)  
}

PointBet <- function(result, point, bet, bet_type) {
  
  if (result == point & bet_type == "pass"){
    bet = bet * 2
  } else if (result == point & bet_type == "dont pass"){
    bet = 0
  } else if (result == 7 & bet_type == "dont pass"){
    bet = bet * 2
  } else if (result == 7 & bet_type == "pass"){
    bet = 0
  }
  
  return(bet)
}


PassBet <- function(result, bet, bet_number, fee){
  if (result == 7){
    if (bet_number == 4 | bet_number == 10){
      bet = (bet * 2) - fee
    } else if (bet_number == 5 | bet_number == 9){
      bet = (bet * (2/3)) - fee
    } else if (bet_number == 6 | bet_number == 8){
      bet = (bet * (5/6)) - fee
    }
  } else if (result == bet_number){
    bet = 0
  }
  
  return(bet)
}
  
#PlaceBet


PlayGame <- function(bet, bet_type,
                     psb=NULL, psb_number=NULL, psb_fee=NULL, 
                     place_bet1 = NULL, place_bet1_number=NULL
                     ){
  
  base_bets <- vector (mode = 'integer')
  psb_winnings <- vector(mode = "integer")
  
  bet_type = 'dont pass'
  
  roll <- RollDie(2)
  
  result <- sum(roll)
  
  bet_init <- bet
  i = 1
  
  bet <- ComeOutBet(result, bet, bet_type)
  
  psb_outcome <- PassBet(result, psb, psb_number, psb_fee)
  
  if (psb_outcome > 0){
    psb_winnings <- append(psb_winnings, psb_outcome)
  } else if (psb_outcome == 0){
    psb <- 0
  }
  
  if (bet_init == bet){
    point <- result
    roll <- RollDie(2)
    result <- sum(roll)
    print(cat("point 1 :", roll, point, sep = " "))
    
    if (psb != 0){
      psb_outcome <- PassBet(result, psb, psb_number, psb_fee)
      
      if (psb_outcome > 0){
        psb_winnings <- append(psb_winnings, psb_outcome)
      } else if (psb_outcome == 0){
        psb <- 0
      }
    }
    
    if (point == result){
      bet <- PointBet(result, point, bet, bet_type)
    } else{
      
      while (result != 7 & result != point){
        roll <- RollDie(2)
        result <- sum(roll)
        
        if (psb != 0){
          psb_outcome <- PassBet(result, psb, psb_number, psb_fee)
          
          if (psb_outcome > 0){
            psb_winnings <- append(psb_winnings, psb_outcome)
          } else if (psb_outcome == 0){
            psb <- 0
          }
        }
        
        print(result)
      }
      
      bet <- PointBet(result, point, bet, bet_type)
      print(cat("point close loop :", roll, point, sep = " "))
      
    }
  }
  
  base_bets <- append(base_bets, bet) 
  
  bet_outcomes <- c(base_bets, psb_winnings)
  
  return(bet_outcomes)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





  


