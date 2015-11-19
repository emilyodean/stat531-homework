create_board = function ()
{
    updated_board = matrix(nrow=5, ncol=5, data=rep(0, 25))
    for (row in 1:5)
    {
        numbers = original_board[row,]
        
        chosen_numbers = sample(1:5, 5, replace=FALSE)
        updated_board[row,chosen_numbers[1]] = original_board[row,chosen_numbers[1]]
        updated_board[row,chosen_numbers[2]] = original_board[row,chosen_numbers[2]]
        
        pulled_balls = c(pulled_balls, original_board[row,chosen_numbers[3:5]])
    }
    return(list(updated_board,pulled_balls))
}

check_for_win = function(game_board)
{
    win = FALSE
    diagonal_win = TRUE 
    for(row in 1:5)
    {
        #check rows
        if (!(0 %in% game_board[row,]))
        {
            win = TRUE
            break
        }
        
        #check columns
        if (!(0 %in% game_board[,row]))
        {
            win = TRUE
            break
        }
        
        #check diagonals
        if(game_board[row, row]==0)
        {
            diagonal_win = FALSE
           
        }
        
        if(game_board[(6-row), row]==0)
        {
            diagonal_win = FALSE
           
        }    
    }
    
    win = diagonal_win | win
    return(win)
}

draw_number = function(balls)
{
    sample(balls, 1, replace=FALSE)
}

update_game_board = function(ball, board)
{
    indices = which(original_board == ball, TRUE)
    #row = as.integer(indices[1])
    #column = as.integer(indices[2])
    board[indices] = ball
    return(board)
}

#set up board
original_board = matrix(nrow=5, ncol=5, data=1:25)

items = create_board()
game_board = matrix(unlist(items[1]), ncol=5)
pulled_balls = c(unlist(items[2]))

win = check_for_win(game_board)

while (win == TRUE)
{
    game_board = create_board()
    win = check_for_win(game_board)
}

#play game
counter = 0
while(win == FALSE)
{
    chosen_ball = draw_number(pulled_balls)
    game_board = update_game_board(chosen_ball, game_board)
    win = check_for_win(game_board)
    pulled_balls = pulled_balls[-which(pulled_balls==chosen_ball)]
    counter = counter+1
}


