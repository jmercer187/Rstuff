library(tidyverse)



#   ROLL DIE FUNC

RollDie <- function(n) {
  sample(1:6,n, replace = T)
}

#   COME OUT FUNC

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

#   POINT BET FUNC

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

#   PASS BET FUNC

PassBet <- function(result, bet, bet_number, vig){
  if (result == 7){
    if (bet_number == 4 | bet_number == 10){
      bet = (bet * 2) - ((bet * 2) * vig)
    } else if (bet_number == 5 | bet_number == 9){
      bet = (bet * (2/3)) - ((bet * (2/3)) * vig)
    } else if (bet_number == 6 | bet_number == 8){
      bet = (bet * (5/6)) - ((bet * (5/6)) * vig)
    }
  } else if (result == bet_number){
    bet = 0
  }
  
  return(bet)
}

#   PLACE BET FUNC

PlaceBet <- function(result, bet, bet_number, vig){

  if (result == 7){
    bet = 0
  } else if (result == 4 | result == 10){
    bet = (bet * (9/5)) - ((bet * (9/5)) * vig)
  } else if (result == 5 | result == 9){
    bet = (bet * (7/5)) - ((bet * (7/5)) * vig)
  } else if (result == 6 | result == 8){
    bet = (bet * (7/6)) - ((bet * (7/6)) * vig)
  } 
  
  return(bet)
}

#   PLAY GAME FUNC

PlayGame <- function(bet, bet_type,
                     psb=NULL, psb_number=NULL, psb_vig=NULL, 
                     plcb = NULL, plcb_number=NULL, plcb_vig=NULL
                     ){
  
  base_bets <- vector (mode = 'integer')
  psb_winnings <- vector(mode = "integer")
  plcb_winnings <- vector(mode = "integer")
  
  bet_type = 'dont pass'
  
  roll <- RollDie(2)
  
  result <- sum(roll)
  
  bet_init <- bet
  i = 1
  
  bet <- ComeOutBet(result, bet, bet_type)
  
  psb_outcome <- PassBet(result, psb, psb_number, psb_vig)
  
  if (psb_outcome > 0){
    psb_winnings <- append(psb_winnings, psb_outcome)
  } else if (psb_outcome == 0){
    psb <- 0
  }
  
  plcb_outcome <- PlaceBet(result, plcb, plcb_number, plcb_vig)
  
  if (plcb_outcome > 0){
    plcb_winnings <- append(plcb_winnings, plcb_outcome)
  } else if (plcb_outcome == 0){
    plcb <- 0
  }
  
  
  if (bet_init == bet){
    point <- result
    roll <- RollDie(2)
    result <- sum(roll)
    print(cat("point 1 :", roll, point, sep = " "))
    
    if (psb != 0){
      psb_outcome <- PassBet(result, psb, psb_number, psb_vig)
      
      if (psb_outcome > 0){
        psb_winnings <- append(psb_winnings, psb_outcome)
      } else if (psb_outcome == 0){
        psb <- 0
      }
    }
    
    if (plcb != 0){
      plcb_outcome <- PlaceBet(result, plcb, plcb_number, plcb_vig)
      
      if (plcb_outcome > 0){
        plcb_winnings <- append(plcb_winnings, plcb_outcome)
      } else if (plcb_outcome == 0){
        plcb <- 0
      }
    }
    
    if (point == result){
      bet <- PointBet(result, point, bet, bet_type)
    } else{
      
      while (result != 7 & result != point){
        roll <- RollDie(2)
        result <- sum(roll)
        
        if (psb != 0){
          psb_outcome <- PassBet(result, psb, psb_number, psb_vig)
          
          if (psb_outcome > 0){
            psb_winnings <- append(psb_winnings, psb_outcome)
          } else if (psb_outcome == 0){
            psb <- 0
          }
        }
        
        if (plcb != 0){
          plcb_outcome <- PlaceBet(result, plcb, plcb_number, plcb_vig)
          
          if (plcb_outcome > 0){
            plcb_winnings <- append(plcb_winnings, plcb_outcome)
          } else if (plcb_outcome == 0){
            plcb <- 0
          }
        }
        
      }
      
      bet <- PointBet(result, point, bet, bet_type)
      print(cat("point close loop :", roll, point, sep = " "))
      
    }
  }
  
  base_bets <- append(base_bets, bet) 
  
  bet_outcomes <- c(base_bets, psb_winnings)
  
  return(bet_outcomes)
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


bet = 20
bet_type = "pass"
place_bet = 0
place_bet_number = 0
place_bet_vig = .05
pass_bet = 35
pass_bet_number = 6
pass_bet_vig = .05
iterations = 1000

game_walk <- vector(mode = "integer")

for (i in seq_along(1:iterations)){
  game_result <- PlayGame(bet, bet_type, place_bet, place_bet_number, place_bet_vig, pass_bet, pass_bet_number, pass_bet_vig)
  
  game_walk <- append(game_walk, game_result)
}

game_walk_df <- as.tibble(game_walk)

# ggplot(data = game_walk_df) +
#   geom_histogram(aes(x = value), binwidth = 6) +
#   geom_vline(xintercept = 85) + 
#   ggtitle("Game Result",
#           subtitle = "Don't Pass = 20, Place on 9 = 30, Pass on 6 = 35")

# ggplot(data = game_walk_df) +
#   geom_histogram(aes(x = value), binwidth = 6) +
#   geom_vline(xintercept = 20) + 
#   ggtitle("Game Result",
#           subtitle = "Don't Pass = 20")

ggplot(data = game_walk_df) +
  geom_histogram(aes(x = value), binwidth = 6) +
  geom_vline(xintercept = 20) +
  ggtitle("Game Result",
          subtitle = "Pass = 20")



  


