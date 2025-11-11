library(ggplot2)

study <- data.frame(
	Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
			3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
	Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

lm_out <- lm(Pass ~ Hours, data = study)
summary(lm_out)

glm_out_norm <- glm(Pass ~ Hours, data = study, family = gaussian())
summary(glm_out_norm)
hist(resid(glm_out_norm))
plot(study$Hours, resid(glm_out_norm))

glm_out_binom <- glm(Pass ~ Hours, data = study, family = binomial(link = 'logit'))
summary(glm_out_binom)
hist(resid(glm_out_binom))

study$predict_norm <- predict(glm_out_norm)
study$predict_binom <- predict(glm_out_binom, type = 'response')

ggplot(study, aes(x = Hours, y = Pass)) +
	geom_point() +
	geom_smooth(method = 'glm', se = FALSE, color = 'blue', formula = y ~ x,
				method.args = list(family = gaussian)) +
	geom_smooth(method = 'glm', se = FALSE, color = 'maroon', formula = y ~ x,
				method.args = list(family = binomial(link='logit'))) +
	geom_segment(aes(xend = Hours, yend = predict_norm),
				 color = 'blue', alpha = 0.5) +
	geom_segment(aes(xend = Hours, yend = predict_binom),
				 color = 'maroon', alpha = 0.5) +
	theme_minimal()

ggplot(study, aes(x = Hours, y = Pass)) +
	geom_point() +
	geom_smooth(method = 'loess', se = FALSE) +
	theme_minimal()


sum(abs(study$Pass - resid(glm_out_norm)))
sum((abs(study$Pass - predict(glm_out_binom, type = 'response'))))

