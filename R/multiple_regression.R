VisualStats::r_squared_shiny()

shiny::runApp(paste0(find.package('VisualStats'), '/shiny/multiple_regression'))

data(depression, package = 'VisualStats')
# Main effects only
lm(depression ~ anxiety + affect, data = depression) |> summary()
# Interaction and main effects
lm(depression ~ anxiety * affect, data = depression) |> summary()
# Interaction effect only
lm(depression ~ anxiety : affect, data = depression) |> summary()


data("poverty", package = 'VisualStats')

lm_out <- lm(poverty ~ female_house, data = poverty)
summary(lm_out)
anova(lm_out)

total_ss <- sum((poverty$poverty - mean(poverty$poverty))^2)
132.57 / total_ss


lm(poverty ~ female_house, data = poverty) |> summary()
lm(poverty ~ white, data = poverty) |> summary()

data(mtcars)
lm1 <- lm(mpg ~ wt, data = mtcars)
lm2 <- lm(mpg ~ wt + hp, data = mtcars)

anova(lm1, lm2)

lm(mpg ~ wt + hp, data = mtcars) |> summary()
lm(mpg ~ wt * hp, data = mtcars) |> summary()
VisualStats::multiple_regression_vis(
	y = mtcars$mpg,
	x1 = mtcars$wt,
	x2 = mtcars$hp,
	plot_residuals = FALSE,
	interaction = TRUE,
	y_lab = 'Miles per Gallon',
	x1_lab = 'Weight',
	x2_lab = 'Horsepower'
)

GGally::ggpairs(mtcars[,c('mpg', 'wt', 'hp')])
