library(DATA606)
shiny_demo('gambler')
shiny_demo('lottery')

sample(letters, size = 30)
sample(letters, size = 30, replace = TRUE)
shiny_demo('DualScales', package='DATA606')
DATA606::shiny_demo('calculus')


rnorm(100) |> hist()
qnorm(.5 )
qnorm(.5, mean = 100, sd = 15)

n <- 10
p <- 0.35
barplot(dbinom(0:n, n, p), names.arg=0:n)
