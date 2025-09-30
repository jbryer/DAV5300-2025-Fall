#' Monty Hall's Game
#'
#' The Monty Hall problem results from a game show, Let's Make a Deal, hosted by Monty Hall. In
#' this game, the player picks one of three doors. Behind one is a car, the other two are goats.
#' After picking a door the player is shown the contents of one of the other two doors, which
#' because the host knows the contents, is a goat. The question to the player: Do you switch your
#' choice.
#'
#' This function will simulate a single play of this game. You can play interactively, or if you
#' specify the `pick` and `switch` parameters this can be looped in order to simulate the results.
#'
#' @references https://en.wikipedia.org/wiki/Monty_Hall_problem
#' @param pick your first pick. Must be A, B, or C.
#' @param swich whether to switch after seeing a door.
#' @return TRUE if you won the game.
#' @examples
#' n_games <- 1000
#' mh_switch <- logical(n_games)
#' mh_no_switch <- logical(n_games)
#' for(i in 1:n_games) {
#' 	pick <- sample(LETTERS[1:3], size = 1)
#' 	mh_switch[i] <- monty_hall(pick = pick, switch = TRUE)
#' 	mh_no_switch[i] <- monty_hall(pick = pick, switch = FALSE)
#' }
#' mean(mh_switch)    # Probability of winning if you always switch.
#' mean(mh_no_switch) # Probability of winning if you do not switch.
monty_hall <- function(pick, switch) {
	interactive <- FALSE
	if(missing(pick)) {
		interactive <- TRUE
		cat('Pick your door:')
		pick <- LETTERS[menu(c('A', 'B', 'C'))]
	} else {
		if(!pick %in% LETTERS[1:3]) {
			stop('pick must be either A, B, or C')
		}
	}
	doors <- c('win', 'lose', 'lose')
	doors <- sample(doors) # Shuffle the doors
	names(doors) <- LETTERS[1:3]
	if(doors[pick] == 'win') {
		show <- sample(names(doors[!names(doors) %in% pick]), size = 1)
	} else {
		show <- doors[!names(doors) %in% pick] == 'lose'
		show <- names(which(show == TRUE))
	}
	if(missing(switch)) {
		interactive <- TRUE
		cat(paste0('Showing door ', show, '. Do you want to switch your choice?'))
		switch <- menu(c('yes', 'no')) == 1
	}
	if(switch) {
		pick <- names(doors)[!names(doors) %in% c(show, pick)]
	}
	win <- unname(doors[pick] == 'win')
	if(interactive) {
		if(win) {
			cat('You win!')
		} else {
			cat('Sorry, you lost.')
		}
		invisible(win)
	} else {
		return(win)
	}
}

monty_hall()
monty_hall("A")

n_games <- 1000
mh_switch <- logical(n_games)
mh_no_switch <- logical(n_games)
for(i in 1:n_games) {
	pick <- sample(LETTERS[1:3], size = 1)
	mh_switch[i] <- monty_hall(pick = pick, switch = TRUE)
	mh_no_switch[i] <- monty_hall(pick = pick, switch = FALSE)
}

mean(mh_switch)
mean(mh_no_switch)
