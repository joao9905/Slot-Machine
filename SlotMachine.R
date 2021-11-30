#defines the probability of each symbol to appear
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}


score <- function(symbols){
    #identifies cases of slot machine
    same <- symbols[1] == symbols[2]&symbols[2] == symbols[3]
    bars <- all(symbols %in% c("B", "BB", "BBB"))
    cherries <- sum(symbols == "C")
    diamonds <- sum(symbols == "DD")
    
    #gives out prize
    if(same){
      payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                   "B" = 10, "C" = 10, "0" = 0)
      prize <- unname(payouts[symbols[1]])
    } else if (bars){
      prize <- 5
    } else{
      cherries <- sum(symbols == "C")
      prize <- c(0, 2, 5)[cherries + 1]
    }
    
    #adjusts the prize by the diamonds
    prize * 2 ^ diamonds
}

play <- function(){
  symbols <-get_symbols()
  print(symbols)
  score(symbols)
}


play()

