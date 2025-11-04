library(ggplot2)

n <- 30
x <- rnorm(n, mean = 100, sd = 15)

hist(x)

( mean_x <- mean(x) )
( sd_x <- sd(x) )

sum((x - mean_x)^2) / (n - 1)
sum((x - mean_x)) / (n - 1)


data(mtcars)
lm_out <- lm(mpg ~ wt, data = mtcars)
summary(lm_out)

x <- mtcars[,'wt']
y <- mtcars[,'mpg']
df <- data.frame(x = x, y = y)

lm_out <- lm(y ~ x)
mean_x <- mean(x)
mean_y <- mean(y)

df$predicted <- predict(lm_out)
df$residual <- df$y - df$predicted
df$residual_squared <- df$residual^2

hist(df$residual_squared)
hist(df$residual)

ggplot(df, aes(x = x, y = y)) +
	geom_smooth(method = 'lm', se = FALSE) +
	geom_point() +
	theme_minimal()

results <- data.frame(r = seq(-1, 1, by=.05),
					  m = as.numeric(NA),
					  b = as.numeric(NA),
					  sumresiduals = as.numeric(NA),
					  sumsquares = as.numeric(NA))
for(i in 1:nrow(results)) {
	results[i,]$m <- results[i,]$r * (mean_y / mean_x)
	results[i,]$b <-  mean_y - results[i,]$m * mean_x
	predicted <- results[i,]$m * x + results[i,]$b
	residual <- y - predicted
	results[i,]$sumresiduals <- sum(residual)
	results[i,]$sumsquares <- sum(residual^2)
}

ggplot(results, aes(x = r, y = sumsquares)) +
	geom_path(color = 'grey50') +
	geom_point(aes(y = sumresiduals), color = 'maroon') +
	geom_point() +
	geom_hline(yintercept = sum(lm_out$residuals^2), color = 'blue') +
	geom_point(x = cor(x, y), y = sum(lm_out$residuals^2), color = 'blue', size = 4) +
	geom_text(x = cor(x, y), y =  sum(lm_out$residuals^2),
			  label = paste0('r = ', round(cor(x, y), digits = 2)),
			  color = 'blue', vjust = -1.7) +
	theme_minimal()



x <- seq(0, 100, 0.5)
df <- data.frame(x = x,
				 log10_x = log10(x),
				 log_x = log(x),
				 log42 = log(x, base = 42)) |>
	reshape2::melt(id.var = 'x')
ggplot(df, aes(x = x, y = value, color = variable)) +
	geom_path() +
	theme_minimal()
