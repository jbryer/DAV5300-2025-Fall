# Why you shouldn't use mean imputation
x <- rnorm(100)
mean(x)
sd(x)

x_miss <- x
x_miss[sample(length(x_miss), 20)] <- NA
x_miss

mean(x_miss)
mean(x_miss, na.rm = TRUE)
sd(x_miss, na.rm = TRUE)

x_miss[is.na(x_miss)] <- mean(x_miss, na.rm = TRUE)
mean(x_miss)
sd(x_miss)


require(Matching)
require(mice)
data(lalonde, package='Matching')

Y <- lalonde$treat
X <- lalonde[,c('age','educ','black','hisp','married','nodegr','re74','re75')]
lalonde.glm <- glm(treat ~ ., family = binomial(link = 'logit'), data = cbind(treat=Y, X))

lalonde.mar <- X
lalonde.nmar <- X

missing.rate <- .2 # What percent of rows will have missing data
missing.cols <- c('nodegr', 're75') # The columns we will add missing values to

# Vectors indicating which rows are treatment and control.
treat.rows <- which(lalonde$treat == 1)
control.rows <- which(lalonde$treat == 0)

set.seed(2112)
for(i in missing.cols) {
	lalonde.mar[sample(nrow(lalonde), nrow(lalonde) * missing.rate), i] <- NA
	lalonde.nmar[sample(treat.rows, length(treat.rows) * missing.rate * 2), i] <- NA
	lalonde.nmar[sample(control.rows, length(control.rows) * missing.rate), i] <- NA
}

shadow.matrix.mar <- as.data.frame(is.na(lalonde.mar))
shadow.matrix.nmar <- as.data.frame(is.na(lalonde.nmar))

names(shadow.matrix.mar) <- names(shadow.matrix.nmar) <- paste0(names(shadow.matrix.mar), '_miss')

set.seed(2112)
mice.mar <- mice(lalonde.mar, m=1)

mice.nmar <- mice(lalonde.nmar, m=1)

complete.mar <- complete(mice.mar)
complete.nmar <- complete(mice.nmar)

lalonde.mar.glm <- glm(treat~.,
					   data=cbind(treat=Y, complete.mar, shadow.matrix.mar))
lalonde.nmar.glm <- glm(treat~.,
						data=cbind(treat=Y, complete.nmar, shadow.matrix.nmar))

summary(lalonde.mar.glm)
summary(lalonde.nmar.glm)




pop <- 1:20
pop
sample(pop) |> unique() |> length()
sample(pop, replace = TRUE) |> unique() |> length()


data(titanic)


df <- data.frame(row.names = 1:100)
for(i in 1:100) {
	samp <- sample(nrow(df), replace = TRUE)
	df[,paste0('samp', i)] <- row.names(df) %in% unique(samp)
}
apply(df, 1, FUN = function(x) { sum(as.integer(x)) / length(x) }) |> mean()


pop <- 1:100
in_bag <- sample(length(pop), length(pop), replace = TRUE)
oob <- pop[!pop %in% in_bag]
oob

library(dplyr)

heart <- read.csv('course_data/heart_attack_predictions.csv')
heart <- heart |>
	mutate_if(is.character, as.numeric) |>
	select(!c(slope, ca, thal))
heart <- heart[complete.cases(heart),]

models <- list()
for(i in 1:100) {
	is <- sample(nrow(heart), nrow(heart), replace = TRUE)
	train <- heart[is,]
	# valid <- heart[-is,]
	out <- glm(num ~ ., data = train, family = binomial(link = logit))
	models[[i]] <- out
	# predictions <- predict(out, newdata = valid, type = 'response')
	# table(predictions)
}

predictions <- data.frame(row.names = 1:nrow(heart))
for(i in 1:length(models)) {
	predictions[,paste0('model', i)] <- predict(models[[i]], newdata = heart, type = 'response')
}
apply(predictions, 1, mean)
