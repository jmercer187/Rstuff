
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


PlayGame <- function(bet, bet_type, iterations, pass_bet=NULL, pass_bet_typ=NULL){
  bet_outcome <- vector(mode = "integer")
  
  for (i in seq_along(1:10)){
    bet = 20
    bet_type = 'dont pass'
    
    roll <- RollDie(2)
    
    result <- sum(roll)
    
    bet_init <- bet
    i = 1
    
    bet <- ComeOutBet(result, bet, bet_type)
    
    if (bet_init == bet){
      point <- result
      roll <- RollDie(2)
      result <- sum(roll)
      print(cat("point 1 :", roll, point, sep = " "))
      
      if (point == result){
        bet <- PointBet(result, point, bet, bet_type)
      } else{
        
        while (result != 7 & result != point){
          roll <- RollDie(2)
          result <- sum(roll)
          print(result)
        }
        
        bet <- PointBet(result, point, bet, bet_type)
        print(cat("point close loop :", roll, point, sep = " "))
        
      }
    }
    bet_outcome <- append(bet_outcome, bet) 
  }
  return(bet_outcome)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



pass_mean <- mean(PlayGame(20, "pass", 1000))
dont_pass_mean <- mean(PlayGame(20, "dont pass", 1000))






  


pass_mean <- mean(bet_outcome)
dontpass_mean <- mean(bet_outcome)



print(result)
