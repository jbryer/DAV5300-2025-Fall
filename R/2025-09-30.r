n <- 1e5
pop <- runif(n, 0, 1)

ci <- data.frame(mean=numeric(), min=numeric(), max=numeric())
for(i in seq_len(100)) {
	samp <- sample(pop, size=30)
	se <- sd(samp) / sqrt(length(samp))
	ci[i,] <- c(mean(samp),
				mean(samp) - 1.96 * se,
				mean(samp) + 1.96 * se)
}
ci$sample <- 1:nrow(ci)
ci$sig <- ci$min < 0.5 & ci$max > 0.5

ggplot(ci, aes(x=min, xend=max, y=sample, yend=sample, color=sig)) +
	geom_vline(xintercept=0.5) +
	geom_segment() + xlab('CI') + ylab('') +
	scale_color_manual(values=c('TRUE'='grey', 'FALSE'='red'))


df <- data.frame(x = seq(1, 10000))
df$se <- 1 / sqrt(df$x)
ggplot(df, aes(x = x, y = se)) + geom_path()
