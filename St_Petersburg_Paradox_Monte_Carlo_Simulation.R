#Part 1 - User To Select Parameters
#Enter Game Fee and Total Number of Games to Play
#GameFee is the cost of game, i.e if game fee is £2 enter 2. 
#TotalGamesToPlay is how many iterations of the Monte Carlo Simulation will run. 

GameFee = 2
TotalGamesToPlay = 10000

#----------------------------------------------------------------------------------------
# Part 2 - Monte Carlo Simulataion
# User does not need modify code. 
#create result table to store data on the outcome of each game. 
result <- data.frame(matrix(nrow = 1, ncol = 5))
colnames(result) <- c("Games_Played", "Prize", "Fees", "Outcome", "Desc")


# run monte carlo simulation of the st petersburg paradox
for (g in 1:TotalGamesToPlay) {
  
  PrizeWon = c()
  Fees = c()
  Outcome = c()
  Desc = c()
  w = 1
  while (sample(c("Heads","Tails"),1, replace = T)!="Heads") {
    w=w+1
  }
  PrizeWon = c(PrizeWon, ifelse(w==0, 0, 2**w))
  Fees = c(Fees, ifelse(w==0, GameFee, GameFee*w))
  Outcome = c(Outcome, PrizeWon - Fees)
  Desc = ifelse(Outcome<0, "Loss",
                ifelse(Outcome==0, "Breakeven", "Profit"))
  
  result[g, 1] = g
  result[g, 2] = PrizeWon
  result[g, 3] = Fees
  result[g, 4] = Outcome
  result[g, 5] = Desc
  
}

#------------------------------------------------------------------------------------------
# Part 3 - Useful Analysis
# User does not need modify code
# This is only to  provide user with useful analysis of the results. 

MeanPrize = mean(result$Prize)
MeanOutcome = mean(result$Outcome)
WorthItRankings = ifelse(MeanOutcome<0, "Loss - Be Cautious,",
                         ifelse(MeanOutcome==0, "Breakeven", "Profit! Yipee you superstar!"))


# show the results table.                       
result

# Create a chart plotting the  financial  outcome of the player after fees in each game played.  Green shows gain or breakeven, red shows a loss. 
par(mar = c(4, 4, 4, 4))
plot(result$Games_Played, result$Outcome, type = "h", lwd=0.5,ylim = c(min(result$Outcome), max(result$Outcome)), col = ifelse(result$Outcome<0, "red", "green"), xlab="Game", ylab="Players Outcome, After Fees", main = "Plot of Players Financial Outcome in Each Game Played", cex.main=1, cex.lab=0.9, cex.axis=0.75)
legend("topleft",c("Gain","Loss"),cex=0.44,col=c("green","red"), pch=c(15,15), pt.cex = 0.5, bg="transparent", text.width=0.15, bty="n")

# Provide Paragraph Describing the financial  Outcome, and Prize. 
print(paste0("The analysis of a  Game Fee of £", GameFee, " and playing ", TotalGamesToPlay, " games. Is the mean prize won was ", round(MeanPrize,digits=2), " After fees the mean financial outcome made is ",round(MeanOutcome,digits=2) , " pounds. This means the odds are you will make a  ", WorthItRankings))
