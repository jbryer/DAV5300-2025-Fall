library(VisualStats)
?variance_vis

x <- rnorm(500)
variance_vis(x, plot_population_variance = TRUE, plot_sample_variance = TRUE)

data(legosets, package = 'brickset')
library(ggplot2)

legosets$US_retailPrice |> is.na() |> table()
ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram()
ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram(binwidth = 25)
ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram(bins = 200)


x <- seq(0, 1000, by = 0.01)
df <- data.frame(x = x,
				 y = log(x),
				 y2 = 1 / x)
ggplot(df, aes(x = x, y = y)) + geom_point()


ggplot(df, aes(x = x, y = y2)) + geom_point()


ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point(aes(color = availability))
ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point()
ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point(color = 'blue')


ggplot(legosets, aes(x = pieces, y = US_retailPrice)) +
	geom_point() +
	geom_vline(xintercept = mean(legosets$pieces, na.rm = TRUE),
			   color = 'purple', size = 2, linetype = 2) +
	geom_hline(yintercept = mean(legosets$US_retailPrice, na.rm = TRUE),
			   color = 'maroon', size = 2, linetype = 3)

