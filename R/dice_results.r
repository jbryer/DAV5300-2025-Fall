library(googlesheets4)
library(dplyr)
library(reshape2)


dice_results_link <- 'https://docs.google.com/spreadsheets/d/15DwdMIBzdA8hvAkXj4BbzQkvpNPnwPG47ns8lyVix6g/edit?resourcekey=&gid=1605667354#gid=1605667354'
dice <- read_sheet(dice_results_link)

dice2 <- dice |>
	select(as.character(1:6))
dice2 <- apply(dice2, 2, sum)

chisq.test(dice2)

library(VisualStats)
distributions_shiny()
qchisq(0.95, df = 5)
qchisq(0.95, df = 3)

1 - pchisq(87, df = 5)

qnorm(0.025)
normal_plot(cv = c(-1.96, 1.96))

