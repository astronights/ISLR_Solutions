# ISLR Answers Practice
### Config 
setwd("C:/Users/shubh31/Desktop/HKU/Seventh Semester/Statistical/")


### Packages Needed
install.packages(c("ISLR", "MASS", "class", "boot", 
                   "leaps", "glmnet", "pls", "gam", 
                   "tree", "randomForest", "gbm", "e1071"))
library(ISLR)
library(MASS)
library(class)
library(boot)
library(leaps)
library(glmnet)
library(pls)
library(gam)
library(splines)
library(tree)
library(randomForest)
library(gbm)
library(e1071)

### Chapter 2
#Q8
college <- read.csv("ISLR-Auto/College.csv")
fix(college)
rownames(college) <- college[,1]
fix(college)
college = college[,-1]
fix(college)
summary(college)
pairs(college[,1:10])
plot(college$Private, college$Outstate)
Elite <- rep("No", nrow(college))
Elite[college$Top10perc>50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate)
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)
par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
plot(college$Accept / college$Apps, college$S.F.Ratio)
plot(college$Top10perc, college$Grad.Rate)

#Q9
Auto = read.csv("ISLR-Auto/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)
sapply(Auto[, 1:7], range)
sapply(Auto[, 1:7], mean)
sapply(Auto[, 1:7], sd)
newAuto = Auto[-(10:85),]
dim(newAuto) == dim(Auto) - c(76,0)
newAuto[9,] == Auto[9,]
newAuto[10,] == Auto[86,]
sapply(newAuto[, 1:7], range)
sapply(newAuto[, 1:7], mean)
sapply(newAuto[, 1:7], sd)
pairs(Auto)
plot(Auto$mpg, Auto$weight)
plot(Auto$mpg, Auto$cylinders)
plot(Auto$mpg, Auto$year)
pairs(Auto)

#Q10
dim(Boston)
names(Boston)
pairs(Boston)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$rad, Boston$crim)
plot(Boston$tax, Boston$crim)
plot(Boston$ptratio, Boston$crim)
par(mfrow=c(1,3))
hist(Boston$crim[Boston$crim>1], breaks=25)
hist(Boston$tax, breaks=25)
hist(Boston$ptratio, breaks=25)
dim(subset(Boston, chas == 1))
median(Boston$ptratio)
t(subset(Boston, medv == min(Boston$medv)))
dim(subset(Boston, rm > 7))
dim(subset(Boston, rm > 8))
summary(subset(Boston, rm > 8))
summary(Boston)


### Chapter 3
#Q8
Auto = read.csv("ISLR-AUto/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
summary(Auto)
lm.fit <- lm(mpg ~ horsepower, data=Auto)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower=c(98)), interval="confidence")
predict(lm.fit, data.frame(horsepower=c(98)), interval="prediction")
par(mfrow=c(1,1))
plot(Auto$horsepower, Auto$mpg)
abline(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

#Q9
pairs(Auto)
cor(subset(Auto, select=-name))
lm.fit1 <- lm(mpg ~ .-name, data=Auto)
summary(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(1,1))
plot(predict(lm.fit1), rstudent(lm.fit1))
lm.fit2 = lm(mpg~cylinders*displacement+displacement*weight, data=Auto)
summary(lm.fit2)
lm.fit3 = lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2), data=Auto)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3)
par(mfrow=c(1,1))
plot(predict(lm.fit3), rstudent(lm.fit3))
lm.fit4<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(lm.fit4)
par(mfrow=c(2,2)) 
plot(lm.fit4)

#Q10
summary(Carseats)
dim(Carseats)
lm.fit = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)
lm.fit2 = lm(Sales ~ Price + US, data=Carseats)
summary(lm.fit2)
confint(lm.fit2)
par(mfrow=c(1,1))
plot(predict(lm.fit2), rstudent(lm.fit2))
par(mfrow=c(2,2))
plot(lm.fit2)

#Q11
par(mfrow=c(1,1))
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
lm.fit = lm(y~x+0)
summary(lm.fit)
lm.fit1 = lm(x~y+0)
summary(lm.fit1)
lm.fit2 = lm(y~x)
lm.fit3 = lm(x~y)
summary(lm.fit2)
summary(lm.fit3)

#Q12
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
set.seed(1)
x <- rnorm(100)
y <- -sample(x, 100)
sum(x^2)
sum(y^2)
lm.fit3 <- lm(y~x+0)
lm.fit4 <- lm(x~y+0)
summary(lm.fit3)
summary(lm.fit4)

#Q13
set.seed(1)
x = rnorm(100)
eps = rnorm(100, 0, sqrt(0.25))
y = -1 + 0.5*x + eps
plot(x, y)
lm.fit = lm(y~x)
summary(lm.fit)
plot(x, y)
abline(lm.fit, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
lm.fit_sq = lm(y~x+I(x^2))
summary(lm.fit_sq)
set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit1 = lm(y1~x1)
summary(lm.fit1)
abline(lm.fit1, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
set.seed(1)
eps2 = rnorm(100, 0, 0.5)
x2 = rnorm(100)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit2 = lm(y2~x2)
summary(lm.fit2)
abline(lm.fit2, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)
confint(lm.fit)
confint(lm.fit1)
confint(lm.fit2)

#Q14
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
cor(x1, x2)
plot(x1, x2)
lm.fit = lm(y~x1+x2)
summary(lm.fit)
lm.fit = lm(y~x1)
summary(lm.fit)
lm.fit = lm(y~x2)
summary(lm.fit)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)
lm.fit2 = lm(y~x1)
summary(lm.fit2)
lm.fit3 = lm(y~x2)
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit3)
par(mfrow=c(1,1))
plot(predict(lm.fit1), rstudent(lm.fit1))
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(predict(lm.fit3), rstudent(lm.fit3))

#Q15
summary(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N","Y"))
summary(Boston)
lm.zn = lm(crim~zn, data=Boston)
summary(lm.zn)
lm.indus = lm(crim~indus, data=Boston)
summary(lm.indus)
lm.chas = lm(crim~chas, data=Boston) 
summary(lm.chas)
lm.nox = lm(crim~nox, data=Boston)
summary(lm.nox)
lm.rm = lm(crim~rm, data=Boston)
summary(lm.rm)
lm.age = lm(crim~age, data=Boston)
summary(lm.age)
lm.dis = lm(crim~dis,data=Boston)
summary(lm.dis)
lm.rad = lm(crim~rad, dat=Boston)
summary(lm.rad)
lm.tax = lm(crim~tax, data=Boston)
summary(lm.tax)
lm.ptratio = lm(crim~ptratio, data=Boston)
summary(lm.ptratio)
lm.black = lm(crim~black, data=Boston)
summary(lm.black)
lm.lstat = lm(crim~lstat, data=Boston)
summary(lm.lstat)
lm.medv = lm(crim~medv, data=Boston)
summary(lm.medv)
lm.all = lm(crim~., data=Boston)
summary(lm.all)
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)
attach(Boston)
lm.zn = lm(crim~poly(zn,3))
summary(lm.zn)
lm.indus = lm(crim~poly(indus,3))
summary(lm.indus)
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) 
lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) 
lm.age = lm(crim~poly(age,3))
summary(lm.age)
lm.dis = lm(crim~poly(dis,3))
summary(lm.dis)
lm.rad = lm(crim~poly(rad,3))
summary(lm.rad)
lm.tax = lm(crim~poly(tax,3))
summary(lm.tax)
lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio)
lm.black = lm(crim~poly(black,3))
summary(lm.black)
lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat)
lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) 


### Chapter 4
#Q10
summary(Weekly)
pairs(Weekly)
cor(Weekly[, -9])
attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
              family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(as.vector(glm.pred), Weekly$Direction)

train = (Weekly$Year < 2009)
Weekly.0910 = Weekly[!train, ]
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Weekly$Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)

lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.0910)
table(lda.pred$class, Direction.0910)
mean(lda.pred$class == Direction.0910)

qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)

train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

glm.fit = glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)

lda.fit = lda(Direction ~ Lag2:Lag1, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.0910)
mean(lda.pred$class == Direction.0910)

qda.fit = qda(Direction ~ Lag2 + sqrt(abs(Lag2)), data = Weekly, subset = train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)

knn.pred = knn(train.X, test.X, train.Direction, k = 10)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

knn.pred = knn(train.X, test.X, train.Direction, k = 100)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

#Q11
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)
cor(Auto[, -9])
pairs(Auto)
train = (year%%2 == 0)
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred = predict(lda.fit, Auto.test)
mean(lda.pred$class != mpg01.test)
qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)
glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != mpg01.test)
# KNN(k=10)
knn.pred = knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred != mpg01.test)
# KNN(k=100)
knn.pred = knn(train.X, test.X, train.mpg01, k = 100)
mean(knn.pred != mpg01.test)

#Q12
Power = function() {
  2^3
}
print(Power())
Power2 = function(x, a) {
  x^a
}
Power2(3, 8)
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)
Power3 = function(x, a) {
  result = x^a
  return(result)
}
x = 1:10
plot(x, Power3(x, 2), log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")
PlotPower = function(x, a) {
  plot(x, Power3(x, a))
}
PlotPower(1:10, 3)

#Q13
attach(Boston)
crime01 = rep(0, length(crim))
crime01[crim > median(crim)] = 1
Boston = data.frame(Boston, crime01)

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]

glm.fit = glm(crime01 ~ . - crime01 - crim, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)
glm.fit = glm(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, family = binomial, 
              subset = train)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != crime01.test)

lda.fit = lda(crime01 ~ . - crime01 - crim, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax, data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

lda.fit = lda(crime01 ~ . - crime01 - crim - chas - tax - lstat - indus - age, 
              data = Boston, subset = train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)

train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                lstat, medv)[train, ]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
               lstat, medv)[test, ]
train.crime01 = crime01[train]
set.seed(1)
# KNN(k=1)
knn.pred = knn(train.X, test.X, train.crime01, k = 1)
mean(knn.pred != crime01.test)
# KNN(k=10)
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)
# KNN(k=100)
knn.pred = knn(train.X, test.X, train.crime01, k = 100)
mean(knn.pred != crime01.test)

train.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[train, ]
test.X = cbind(zn, nox, rm, dis, rad, ptratio, black, medv)[test, ]
knn.pred = knn(train.X, test.X, train.crime01, k = 10)
mean(knn.pred != crime01.test)

### Chapter 5
#Q5
summary(Default)
attach(Default)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)
FiveB = function() {
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  # ii.
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != Default[-train, ]$default))
}
FiveB()
train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)

#Q6
boot.fn = function(data, index) {
  return(coef(glm(default ~ income + balance, data = data, family = binomial, subset = index)))
}

boot(Default, boot.fn, 50)


#Q7
attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(glm.fit)
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
summary(glm.fit)
predict.glm(glm.fit, Weekly[1, ], type = "response") > 0.5
Weekly[1,]$Direction
count = rep(0, dim(Weekly)[1])
for (i in 1:(dim(Weekly)[1])) {
  glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
  is_up = predict.glm(glm.fit, Weekly[i, ], type = "response") > 0.5
  is_true_up = Weekly[i, ]$Direction == "Up"
  if (is_up != is_true_up) 
    count[i] = 1
}
sum(count)
mean(count)

#Q8
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)

plot(x, y)
Data = data.frame(x, y)
set.seed(1)
# i.
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta

glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta

summary(glm.fit)

#Q9
attach(Boston)
medv.mean = mean(medv)
medv.mean
medv.err = sd(medv)/sqrt(length(medv))
medv.err
boot.fn = function(data, index) return(mean(data[index]))
bstrap = boot(medv, boot.fn, 1000)
bstrap
t.test(medv)
c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119)
medv.med = median(medv)
medv.med
boot.fn = function(data, index) return(median(data[index]))
boot(medv, boot.fn, 1000)
medv.tenth = quantile(medv, c(0.1))
medv.tenth
boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)


### Chapter 6
#Q8
set.seed(1)
X = rnorm(100)
eps = rnorm(100)
beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)

# Find the model size for best cp, BIC and adjr2
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)

plot(mod.summary$cp, xlab = "Subset Size", ylab = "Cp", pch = 20, type = "l")
points(3, mod.summary$cp[3], pch = 4, col = "red", lwd = 7)

plot(mod.summary$bic, xlab = "Subset Size", ylab = "BIC", pch = 20, type = "l")
points(3, mod.summary$bic[3], pch = 4, col = "red", lwd = 7)

plot(mod.summary$adjr2, xlab = "Subset Size", ylab = "Adjusted R2", pch = 20, 
     type = "l")
points(3, mod.summary$adjr2[3], pch = 4, col = "red", lwd = 7)

coefficients(mod.full, id = 3)

mod.fwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, 
                     method = "forward")
mod.bwd = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10, 
                     method = "backward")
fwd.summary = summary(mod.fwd)
bwd.summary = summary(mod.bwd)
which.min(fwd.summary$cp)
which.min(bwd.summary$cp)
which.min(fwd.summary$bic)
which.min(bwd.summary$bic)
which.max(fwd.summary$adjr2)
which.max(bwd.summary$adjr2)

par(mfrow = c(3, 2))
plot(fwd.summary$cp, xlab = "Subset Size", ylab = "Forward Cp", pch = 20, type = "l")
points(3, fwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$cp, xlab = "Subset Size", ylab = "Backward Cp", pch = 20, type = "l")
points(3, bwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$bic, xlab = "Subset Size", ylab = "Forward BIC", pch = 20, 
     type = "l")
points(3, fwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$bic, xlab = "Subset Size", ylab = "Backward BIC", pch = 20, 
     type = "l")
points(3, bwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$adjr2, xlab = "Subset Size", ylab = "Forward Adjusted R2", 
     pch = 20, type = "l")
points(3, fwd.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$adjr2, xlab = "Subset Size", ylab = "Backward Adjusted R2", 
     pch = 20, type = "l")
points(4, bwd.summary$adjr2[4], pch = 4, col = "red", lwd = 7)

coefficients(mod.fwd, id = 3)
coefficients(mod.bwd, id = 3)
coefficients(mod.fwd, id = 4)

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]
mod.lasso = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = mod.lasso$lambda.min
best.lambda
par(mfrow=c(1,1))
plot(mod.lasso)

best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

beta7 = 7
Y = beta0 + beta7 * X^7 + eps
# Predict using regsubsets
data.full = data.frame(y = Y, x = X)
mod.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
mod.summary = summary(mod.full)

# Find the model size for best cp, BIC and adjr2
which.min(mod.summary$cp)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id = 1)
coefficients(mod.full, id = 2)
coefficients(mod.full, id = 4)

xmat = model.matrix(y ~ poly(x, 10, raw = T), data = data.full)[, -1]
mod.lasso = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = mod.lasso$lambda.min
best.lambda
best.model = glmnet(xmat, Y, alpha = 1)
predict(best.model, s = best.lambda, type = "coefficients")

#Q9
set.seed(11)
sum(is.na(College))

train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]

lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
mean((College.test[, "Apps"] - lm.pred)^2)

train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best

ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)

mod.lasso = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best

lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)

mod.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(mod.lasso, s=lambda.best, type="coefficients")

pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pcr.pred))^2)
pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, College.test, ncomp=10)
mean((College.test[, "Apps"] - data.frame(pls.pred))^2)
test.avg <- mean(College.test[, "Apps"])
lm.test.r2 = 1 - mean((College.test[, "Apps"] - lm.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((College.test[, "Apps"] - ridge.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((College.test[, "Apps"] - lasso.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pcr.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pls.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")

#Q10
set.seed(1)
p = 20
n = 1000
x = matrix(rnorm(n * p), n, p)
B = rnorm(p)
B[3] = 0
B[4] = 0
B[9] = 0
B[19] = 0
B[10] = 0
eps = rnorm(p)
y = x %*% B + eps

train = sample(seq(1000), 100, replace = FALSE)
y.train = y[train, ]
y.test = y[-train, ]
x.train = x[train, ]
x.test = x[-train, ]


regfit.full = regsubsets(y ~ ., data = data.frame(x = x.train, y = y.train), 
                         nvmax = p)
val.errors = rep(NA, p)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = as.matrix(x.train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                    x_cols]
  val.errors[i] = mean((y.train - pred)^2)
}
plot(val.errors, ylab = "Training MSE", pch = 19, type = "b")

val.errors = rep(NA, p)
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = as.matrix(x.test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                   x_cols]
  val.errors[i] = mean((y.test - pred)^2)
}
plot(val.errors, ylab = "Test MSE", pch = 19, type = "b")

which.min(val.errors)
coef(regfit.full, id = 16)

val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  a[i] = length(coefi) - 1
  b[i] = sqrt(sum((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + 
                sum(B[!(x_cols %in% names(coefi))])^2)
}
plot(x = a, y = b, xlab = "number of coefficients", ylab = "error between estimated and true coefficients")
which.min(b)

#Q11
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(Boston) - 1
folds = sample(rep(1:k, length = nrow(Boston)))
cv.errors = matrix(NA, k, p)
for (i in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != i, ], nvmax = p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds == i, ], id = j)
    cv.errors[i, j] = mean((Boston$crim[folds == i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch = 19, type = "b")

which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]

x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.lasso = cv.glmnet(x, y, type.measure = "mse")
plot(cv.lasso)

coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim
cv.ridge = cv.glmnet(x, y, type.measure = "mse", alpha = 0)
plot(cv.ridge)
coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)
coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)


### Chapter 7
#Q6
set.seed(1)
all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(wage~poly(age, i), data=Wage)
  all.deltas[i] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}
plot(1:10, all.deltas, xlab="Degree", ylab="CV error", type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(all.deltas)
sd.points = sd(all.deltas)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)


plot(wage~age, data=Wage, col="darkgrey")
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit = lm(wage~poly(age, 3), data=Wage)
lm.pred = predict(lm.fit, data.frame(age=age.grid))
lines(age.grid, lm.pred, col="blue", lwd=2)

all.cvs = rep(NA, 10)
for (i in 2:10) {
  Wage$age.cut = cut(Wage$age, i)
  lm.fit = glm(wage~age.cut, data=Wage)
  all.cvs[i] = cv.glm(Wage, lm.fit, K=10)$delta[2]
}
plot(2:10, all.cvs[-1], xlab="Number of cuts", ylab="CV error", type="l", pch=20, lwd=2)

lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)

#Q7
summary(Wage$maritl)
summary(Wage$jobclass)
par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)
fit = lm(wage ~ maritl, data = Wage)
deviance(fit)
fit = lm(wage ~ jobclass, data = Wage)
deviance(fit)
fit = lm(wage ~ maritl + jobclass, data = Wage)
deviance(fit)

fit = gam(wage ~ maritl + jobclass + s(age, 4), data = Wage)
deviance(fit)

#Q8
set.seed(1)
rss = rep(NA, 10)
fits = list()
for (d in 1:10) {
  fits[[d]] = lm(mpg ~ poly(displacement, d), data = Auto)
  rss[d] = deviance(fits[[d]])
}
rss
anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]])
cv.errs = rep(NA, 15)
for (d in 1:15) {
  fit = glm(mpg ~ poly(displacement, d), data = Auto)
  cv.errs[d] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)
cv.errs
cv.errs = rep(NA, 10)
for (c in 2:10) {
  Auto$dis.cut = cut(Auto$displacement, c)
  fit = glm(mpg ~ dis.cut, data = Auto)
  cv.errs[c] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)
cv.errs

cv.errs = rep(NA, 10)
for (df in 3:10) {
  fit = glm(mpg ~ ns(displacement, df = df), data = Auto)
  cv.errs[df] = cv.glm(Auto, fit, K = 10)$delta[2]
}
which.min(cv.errs)

fit = gam(mpg ~ s(displacement, 4) + s(horsepower, 4), data = Auto)
summary(fit)

#Q9
lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
summary(lm.fit)
dislim = range(dis)
dis.grid = seq(from = dislim[1], to = dislim[2], by = 0.1)
lm.pred = predict(lm.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, lm.pred, col = "red", lwd = 2)
all.rss = rep(NA, 10)
for (i in 1:10) {
  lm.fit = lm(nox ~ poly(dis, i), data = Boston)
  all.rss[i] = sum(lm.fit$residuals^2)
}
all.rss

all.deltas = rep(NA, 10)
for (i in 1:10) {
  glm.fit = glm(nox ~ poly(dis, i), data = Boston)
  all.deltas[i] = cv.glm(Boston, glm.fit, K = 10)$delta[2]
}
plot(1:10, all.deltas, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)

sp.fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)
summary(sp.fit)

sp.pred = predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)

all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = lm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = sum(lm.fit$residuals^2)
}
all.cv[-c(1, 2)]

all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}

plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")

#Q10
attach(College)
train = sample(length(Outstate), length(Outstate)/2)
test = -train
College.train = College[train, ]
College.test = College[test, ]
reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
reg.summary = summary(reg.fit)
par(mfrow = c(1, 3))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + 
                s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College.train)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")

gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err

gam.tss = mean((College.test$Outstate - mean(College.test$Outstate))^2)
test.rss = 1 - gam.err/gam.tss
test.rss

summary(gam.fit)

#Q11
set.seed(1)
X1 = rnorm(100)
X2 = rnorm(100)
eps = rnorm(100, sd = 0.1)
Y = -2.1 + 1.3 * X1 + 0.54 * X2 + eps
beta0 = rep(NA, 1000)
beta1 = rep(NA, 1000)
beta2 = rep(NA, 1000)
beta1[1] = 10
par(mfrow=c(1,1))
for (i in 1:1000) {
  a = Y - beta1[i] * X1
  beta2[i] = lm(a ~ X2)$coef[2]
  a = Y - beta2[i] * X2
  lm.fit = lm(a ~ X1)
  if (i < 1000) {
    beta1[i + 1] = lm.fit$coef[2]
  }
  beta0[i] = lm.fit$coef[1]
}
plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
legend("center", c("beta0", "beta1", "beta2"), lty = 1, col = c("green", "red", 
                                                                "blue"))

lm.fit = lm(Y ~ X1 + X2)
plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
abline(h = lm.fit$coef[1], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[2], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[3], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
legend("center", c("beta0", "beta1", "beta2", "multiple regression"), lty = c(1, 
                                                                              1, 1, 2), col = c("green", "red", "blue", "black"))

#Q12
set.seed(1)
p = 100
n = 1000
x = matrix(ncol = p, nrow = n)
coefi = rep(NA, p)
for (i in 1:p) {
  x[, i] = rnorm(n)
  coefi[i] = rnorm(1) * 100
}
y = x %*% coefi + rnorm(n)

beta = rep(0, p)
max_iterations = 1000
errors = rep(NA, max_iterations + 1)
iter = 2
errors[1] = Inf
errors[2] = sum((y - x %*% beta)^2)
threshold = 1e-04
while (iter < max_iterations && errors[iter - 1] - errors[iter] > threshold) {
  for (i in 1:p) {
    a = y - x %*% beta + beta[i] * x[, i]
    beta[i] = lm(a ~ x[, i])$coef[2]
  }
  iter = iter + 1
  errors[iter] = sum((y - x %*% beta)^2)
  print(c(iter - 2, errors[iter - 1], errors[iter]))
}

plot(1:11, errors[3:13])

### Chapter 8
#Q7
train = sample(dim(Boston)[1], dim(Boston)[1]/2)
X.train = Boston[train, -14]
X.test = Boston[-train, -14]
Y.train = Boston[train, 14]
Y.test = Boston[-train, 14]

p = dim(Boston)[2] - 1
p.2 = p/2
p.sq = sqrt(p)

rf.boston.p = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                           mtry = p, ntree = 500)
rf.boston.p.2 = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                             mtry = p.2, ntree = 500)
rf.boston.p.sq = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                              mtry = p.sq, ntree = 500)

plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.p.2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.p.sq$test$mse, col = "blue", type = "l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
       cex = 1, lty = 1)

#Q8
train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)

cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

pruned.carseats = prune.tree(tree.carseats, best = 14)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)

bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, 
                            importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)
importance(bag.carseats)
rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
                           importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)
importance(rf.carseats)

#Q9
attach(OJ)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]
oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

oj.tree

plot(oj.tree)
text(oj.tree, pretty = 0)

oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

cv.oj = cv.tree(oj.tree, FUN = prune.tree)
plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")

oj.pruned = prune.tree(oj.tree, best = 6)
summary(oj.pruned)

pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)

#Q10
sum(is.na(Hitters$Salary))
Hitters = Hitters[-which(is.na(Hitters$Salary)), ]
sum(is.na(Hitters$Salary))
Hitters$Salary = log(Hitters$Salary)

train = 1:200
Hitters.train = Hitters[train, ]
Hitters.test = Hitters[-train, ]

pows = seq(-10, -0.2, by = 0.1)
lambdas = 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
for (i in 1:length.lambdas) {
  boost.hitters = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                      n.trees = 1000, shrinkage = lambdas[i])
  train.pred = predict(boost.hitters, Hitters.train, n.trees = 1000)
  test.pred = predict(boost.hitters, Hitters.test, n.trees = 1000)
  train.errors[i] = mean((Hitters.train$Salary - train.pred)^2)
  test.errors[i] = mean((Hitters.test$Salary - test.pred)^2)
}

plot(lambdas, train.errors, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "blue", pch = 20)

plot(lambdas, test.errors, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "red", pch = 20)
min(test.errors)
lambdas[which.min(test.errors)]
lm.fit = lm(Salary ~ ., data = Hitters.train)
lm.pred = predict(lm.fit, Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)

x = model.matrix(Salary ~ ., data = Hitters.train)
y = Hitters.train$Salary
x.test = model.matrix(Salary ~ ., data = Hitters.test)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Hitters.test$Salary - lasso.pred)^2)

boost.best = gbm(Salary ~ ., data = Hitters.train, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(boost.best)
rf.hitters = randomForest(Salary ~ ., data = Hitters.train, ntree = 500, mtry = 19)
rf.pred = predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary - rf.pred)^2)

#Q11
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, 
                    distribution = "bernoulli")
summary(boost.caravan)
boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)
lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred)

#Q12
summary(Weekly)
train = sample(nrow(Weekly), 2/3 * nrow(Weekly))
test = -train
glm.fit = glm(Direction ~ . - Year - Today, data = Weekly[train, ], family = "binomial")
glm.probs = predict(glm.fit, newdata = Weekly[test, ], type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly$Direction[test])
mean(glm.pred != Weekly$Direction[test])

Weekly$BinomialDirection = ifelse(Weekly$Direction == "Up", 1, 0)
boost.weekly = gbm(BinomialDirection ~ . - Year - Today - Direction, data = Weekly[train, 
                                                                                   ], distribution = "bernoulli", n.trees = 5000)
yhat.boost = predict(boost.weekly, newdata = Weekly[test, ], n.trees = 5000)
yhat.pred = rep(0, length(yhat.boost))
yhat.pred[yhat.boost > 0.5] = 1
table(yhat.pred, Weekly$BinomialDirection[test])
mean(yhat.pred != Weekly$BinomialDirection[test])

Weekly = Weekly[, !(names(Weekly) %in% c("BinomialDirection"))]
bag.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                          mtry = 6)
yhat.bag = predict(bag.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
mean(yhat.bag != Weekly$Direction[test])

rf.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, 
                         mtry = 2)
yhat.bag = predict(rf.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])
mean(yhat.bag != Weekly$Direction[test])

### Chapter 9
#Q4
x = rnorm(100)
y = 3 * x^2 + 4 + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 3
y[-train] = y[-train] - 3
# Plot using different colors
plot(x[train], y[train], pch="+", lwd=4, col="red", ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="blue")
z = rep(0, 100)
z[train] = 1
# Take 25 observations each from train and -train
final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data.train = data.frame(x=x[final.train], y=y[final.train], z=as.factor(z[final.train]))
data.test = data.frame(x=x[-final.train], y=y[-final.train], z=as.factor(z[-final.train]))
svm.linear = svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)
table(z[final.train], predict(svm.linear, data.train))
svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)
table(z[final.train], predict(svm.poly, data.train))
svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, cost=10)
plot(svm.radial, data.train)
table(z[final.train], predict(svm.radial, data.train))
plot(svm.linear, data.test)
plot(svm.poly, data.test)
plot(svm.radial, data.test)
table(z[-final.train], predict(svm.linear, data.test))
table(z[-final.train], predict(svm.poly, data.test))
table(z[-final.train], predict(svm.radial, data.test))

#Q5
x1 = runif(500) - 0.5
x2 = runif(500) - 0.5
y = 1 * (x1^2 - x2^2 > 0)
plot(x1[y == 0], x2[y == 0], col = "red", xlab = "X1", ylab = "X2", pch = "+")
points(x1[y == 1], x2[y == 1], col = "blue", pch = 4)
lm.fit = glm(y ~ x1 + x2, family = binomial)
summary(lm.fit)
data = data.frame(x1 = x1, x2 = x2, y = y)
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.52, 1, 0)
data.pos = data[lm.pred == 1, ]
data.neg = data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)
lm.fit = glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), data = data, family = binomial)
lm.prob = predict(lm.fit, data, type = "response")
lm.pred = ifelse(lm.prob > 0.5, 1, 0)
data.pos = data[lm.pred == 1, ]
data.neg = data[lm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

svm.fit = svm(as.factor(y) ~ x1 + x2, data, kernel = "linear", cost = 0.1)
svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.neg = data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

svm.fit = svm(as.factor(y) ~ x1 + x2, data, gamma = 1)
svm.pred = predict(svm.fit, data)
data.pos = data[svm.pred == 1, ]
data.neg = data[svm.pred == 0, ]
plot(data.pos$x1, data.pos$x2, col = "blue", xlab = "X1", ylab = "X2", pch = "+")
points(data.neg$x1, data.neg$x2, col = "red", pch = 4)

#Q6
x.one = runif(500, 0, 90)
y.one = runif(500, x.one + 10, 100)
x.one.noise = runif(50, 20, 80)
y.one.noise = 5/4 * (x.one.noise - 10) + 0.1

# Class zero
x.zero = runif(500, 10, 100)
y.zero = runif(500, 0, x.zero - 10)
x.zero.noise = runif(50, 20, 80)
y.zero.noise = 5/4 * (x.zero.noise - 10) - 0.1

# Combine all
class.one = seq(1, 550)
x = c(x.one, x.one.noise, x.zero, x.zero.noise)
y = c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)
z = rep(0, 1100)
z[class.one] = 1
data = data.frame(x = x, y = y, z = z)
tune.out = tune(svm, as.factor(z) ~ ., data = data, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                              0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)
data.frame(cost = tune.out$performances$cost, misclass = tune.out$performances$error * 
             1100)
x.test = runif(1000, 0, 100)
class.one = sample(1000, 500)
y.test = rep(NA, 1000)
# Set y > x for class.one
for (i in class.one) {
  y.test[i] = runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {
  y.test[i] = runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)

z.test = rep(0, 1000)
z.test[class.one] = 1
all.costs = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.errors = rep(NA, 8)
data.test = data.frame(x = x.test, y = y.test, z = z.test)
for (i in 1:length(all.costs)) {
  svm.fit = svm(as.factor(z) ~ ., data = data, kernel = "linear", cost = all.costs[i])
  svm.predict = predict(svm.fit, data.test)
  test.errors[i] = sum(svm.predict != data.test$z)
}

data.frame(cost = all.costs, `test misclass` = test.errors)

#Q7
gas.med = median(Auto$mpg)
new.var = ifelse(Auto$mpg > gas.med, 1, 0)
Auto$mpglevel = as.factor(new.var)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 
                                                                                          0.1, 1, 5, 10, 100)))
summary(tune.out)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.1, 
                                                                                              1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.1, 
                                                                                          1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
svm.linear = svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 10, 
               degree = 2)
svm.radial = svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 10, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)
plotpairs(svm.poly)
plotpairs(svm.radial)

#Q8
train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]
svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = 0.01)
summary(svm.linear)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)

svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)

train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial", ranges = list(cost = 10^seq(-2, 
                                                                                                   1, by = 0.25)))
summary(tune.out)

svm.radial = svm(Purchase ~ ., data = OJ.train, kernel = "radial", cost = tune.out$best.parameters$cost)
train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2)
summary(svm.poly)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)

tune.out = tune(svm, Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

svm.poly = svm(Purchase ~ ., data = OJ.train, kernel = "poly", degree = 2, cost = tune.out$best.parameters$cost)
train.pred = predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
test.pred = predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
### Chapter 10
#Q7
dsc = scale(USArrests)
a = dist(dsc)^2
b = as.dist(1 - cor(t(dsc)))
summary(b/a)

#Q8
pr.out = prcomp(USArrests, center=T, scale=T)
pr.var = pr.out$sdev^2
pve = pr.var / sum(pr.var)
pve

loadings = pr.out$rotation
pve2 = rep(NA, 4)
dmean = apply(USArrests, 2, mean)
dsdev = sqrt(apply(USArrests, 2, var))
dsc = sweep(USArrests, MARGIN=2, dmean, "-")
dsc = sweep(dsc, MARGIN=2, dsdev, "/")
for (i in 1:4) {
  proto_x = sweep(dsc, MARGIN=2, loadings[,i], "*")
  pc_x = apply(proto_x, 1, sum)
  pve2[i] = sum(pc_x^2)
}
pve2 = pve2/sum(dsc^2)
pve2

#Q9
hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)

cutree(hc.complete, 3)

table(cutree(hc.complete, 3))

dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)

cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))

#Q10
x = matrix(rnorm(20*3*50, mean=0, sd=0.001), ncol=50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1

pca.out = prcomp(x)
summary(pca.out)

pca.out$x[,1:2]

plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 

km.out = kmeans(x, 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

km.out = kmeans(x, 2, nstart=20)
km.out$cluster

km.out = kmeans(x, 4, nstart=20)
km.out$cluster

km.out = kmeans(pca.out$x[,1:2], 3, nstart=20)
table(km.out$cluster, c(rep(1,20), rep(2,20), rep(3,20)))

km.out = kmeans(scale(x), 3, nstart=20)
km.out$cluster

#Q11
data = read.csv("ISLR-Auto/Ch10Ex11.csv", header=F)
dim(data)
dd = as.dist(1 - cor(data))
plot(hclust(dd, method="complete"))
plot(hclust(dd, method="single"))
plot(hclust(dd, method="average"))
pr.out = prcomp(t(data))
summary(pr.out)

total_load = apply(pr.out$rotation, 1, sum)
indices = order(abs(total_load), decreasing=T)
indices[1:10]
total_load[indices[1:10]]
