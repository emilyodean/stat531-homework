#8.6

totalWins = 0
numGames = NULL
for(i in 1:999)
{
    wins = 0
    losses = 0
    gamesPlayed = 0
    while(wins < 4 & losses < 4)
    {
        if(gamesPlayed ==1 | gamesPlayed ==2 | gamesPlayed == 6 | gamesPlayed == 7)
        {
            wins = playHomeGame(wins)
        } else
        {
            wins = playAwayGame(wins)
        }
        gamesPlayed = gamesPlayed + 1
        losses = gamesPlayed - wins
    }
    if (wins == 4)
    {
        totalWins = totalWins + 1
    }
    numGames = c(numGames, gamesPlayed)
}

totalWins/999
hist(numGames)



playHomeGame = function(winning)
{
    win1 = sample(1:5, 1)
    if (win1 != 5)
    {
        wins = wins+1
    }
    return(wins)
}

playAwayGame = function(winning)
{
    win1 = sample(1:5, 1)
    if (win1 < 3)
    {
        wins = wins+1
    }
    return(wins)
}