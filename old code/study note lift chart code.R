
# Using the car.csv data from the book
# Generalized Linear Models for Insurance Data
# by Piet de Jong and Gillian Heller
# http://www.acst.mq.edu.au/GLMsforInsuranceData/
#
dta <- read.csv("data/car.csv")
dta <- dta[,1:10]
dta$veh_age <- as.factor(dta$veh_age)
dta$agecat <- as.factor(dta$agecat)
# separate all records with positive claims cost
idx <- dta$claimcst0 > 0
dtb <- dta[idx,]
dtb$obs <- 1:(dim(dtb)[1]) #adding row number
rm(idx, dta)
# add random numbers and split data into training and holdout
set.seed(189763)
dtb$rnd <- runif(dim(dtb)[1])
idx <- dtb$rnd < 0.6
dt.train <- dtb[idx,]
dt.holdout <- dtb[!idx,]
rm(idx, dtb)
#
# Fit a simple gamma GLM with two predictors
# This will be our current base model
#
fit.base <- glm(claimcst0 ~ area + veh_age, data = dt.train,
                family = Gamma(link = "log"))
dt.holdout$base <- predict(fit.base, newdata = dt.holdout, type = "response")
#
# Fit a "superior" model by including a new variable
# Store our model predictions in the dataset
#
fit.A <- glm(claimcst0 ~ area + veh_age + gender, data = dt.train,
             family = Gamma(link = "log"))
dt.holdout$mod.A <- predict(fit.A, newdata = dt.holdout, type = "response")
#
# Calculate bins of equal exposure based on sorting
# by model A predictions
#
set.seed(13472)
ord <- order(dt.holdout$mod.A, runif(length(dt.holdout$mod.A))) #second random vector breaks ties in first vector. returns row numbers
dt.holdout <- dt.holdout[ord,] #sort
cum.expo <- cumsum(dt.holdout$exposure)
total.exposure <- sum(dt.holdout$exposure)
bks <- c(0, 1:9 * total.exposure/10, 10.2 * total.exposure/10)
dt.holdout$bin.A <- cut(cum.expo, breaks = bks, labels = 1:10)
dt.holdout <- dt.holdout[order(dt.holdout$obs),]

rm(ord, cum.expo, total.exposure, bks)
# Check that summing exposure by bin number gives about
# equal exposure in each bin
tapply(dt.holdout$exposure, dt.holdout$bin.A, sum)
#
# Now let's calculate the lift that the new model provides
# compared to the current base model using deciles
#
avg.mod.A <- tapply(dt.holdout$mod.A,dt.holdout$bin.A, mean) / mean(dt.holdout$mod.A)
avg.actual <- tapply(dt.holdout$claimcst0,
                     dt.holdout$bin.A, mean)/mean(dt.holdout$mod.A)
#I think above line is an error. emailed CAS. should be this: 
avg.actual <- tapply(dt.holdout$claimcst0,
                     dt.holdout$bin.A, mean)/mean(dt.holdout$claimcst0)

par(mar = c(4,4,1,1)+0.1)
lim = c(0.5, 1.65)
plot(x = 1:10, y = avg.actual, type = "b", ylim = lim,
     xlab = "Deciles", ylab = "")
points(x = 1:10, y = avg.mod.A, pch = 16, col = "red")
lines(x = 1:10, y = avg.mod.A, col = "red")
text(x = 8, y = avg.actual[8], labels = "Actual", pos = 1)
text(x = 4, y = avg.mod.A[4], labels = "Model A", pos = 3, col = "red")
abline(h = avg.mod.A[c(1,10)], lty = 3, col = "red")
arrows(x0 = 1.5, y0 = avg.mod.A[1], x1 = 1.5, y1 = avg.mod.A[10],
       code = 3, col = "red", angle = 15, length = 0.1)
text(x = 1.5, y = 1.2,
     labels = paste(c("Lift = ",
                      round(avg.mod.A[10],2), " - ",
                      round(avg.mod.A[1],2), " = ",
                      round(diff(avg.mod.A[c(1,10)]),2)),
                    collapse = ""),
     pos = 4, cex = 0.8, col = "red")
rm(avg.mod.A, avg.actual, lim)
#
# Fit a complete model by including all variables
#
fit.B <- glm(claimcst0 ~ area + veh_age + gender + agecat +
               veh_value + veh_body,
             data = dt.train, family = Gamma(link = "log"))
dt.holdout$mod.B <- predict(fit.B, newdata = dt.holdout, type = "response")
#
# Calculate bins of equal exposure based on sorting
# by model B predictions
#
set.seed(13472)
ord <- order(dt.holdout$mod.B, runif(length(dt.holdout$mod.B)))
dt.holdout <- dt.holdout[ord,]
cum.expo <- cumsum(dt.holdout$exposure)
total.exposure <- sum(dt.holdout$exposure)
bks <- c(0, 1:9 * total.exposure/10, 10.2 * total.exposure/10)

dt.holdout$bin.B <- cut(cum.expo, breaks = bks, labels = 1:10)
dt.holdout <- dt.holdout[order(dt.holdout$obs),]
rm(ord, cum.expo, total.exposure, bks)
# Check that summing exposure by bin number gives about
# equal exposure in each bin
tapply(dt.holdout$exposure, dt.holdout$bin.B, sum)
#
# Now let's calculate the lift that the new model provides
# compared to the current base model using deciles
#
avg.mod.B <- tapply(dt.holdout$mod.B,
                    dt.holdout$bin.B, mean)/mean(dt.holdout$mod.B)
avg.actual <- tapply(dt.holdout$claimcst0,
                     dt.holdout$bin.B, mean)/mean(dt.holdout$mod.B)
par(mar = c(4,4,1,1)+0.1)
lim = c(0.5, 1.65)
plot(x = 1:10, y = avg.actual, type = "b", ylim = lim,
     xlab = "Deciles", ylab = "")
points(x = 1:10, y = avg.mod.B, pch = 16, col = "red")
lines(x = 1:10, y = avg.mod.B, col = "red")
text(x = 7, y = avg.actual[7], labels = "Actual", pos = 1)
text(x = 9, y = avg.mod.B[9], labels = "Model B", pos = 2, col = "red")
abline(h = avg.mod.B[c(1,10)], lty = 3, col = "red")
arrows(x0 = 1.5, y0 = avg.mod.B[1], x1 = 1.5, y1 = avg.mod.B[10],
       code = 3, col = "red", angle = 15, length = 0.1)
text(x = 1.5, y = 1.4,
     labels = paste(c("Lift = ",
                      round(avg.mod.B[10],2), " - ",
                      round(avg.mod.B[1],2), " = ",
                      round(diff(avg.mod.B[c(1,10)]),2)),
                    collapse = ""),
     pos = 4, cex = 0.8, col = "red")
rm(avg.mod.B, avg.actual, lim)



#double lift


#ROC

library(foreign)
library(rms)
# Set to TRUE to generate the graph as a JPEG file
generate.file <- FALSE
#
# Using the testicular cancer (t821.sav) data from the book
# Clinical Prediction Models
# by Ewout W. Steyerberg
# http://www.clinicalpredictionmodels.org/
#
tmp <- read.spss("data/t821.sav", to.data.frame = TRUE)
# The data t821.sav is in SPSS format and reading it will
# throw a warning message about undeclared levels in the
# HOSP variable. Please ignore it as we are not using
# this variable for this example.
n544 <- tmp[tmp$STUDY == "development",]
#
# Set the vector Y to Yes or No depending
# on whether the patient has benign tissue
#
Y <- rep("No", 544)
Y[n544$NEC == 1] <- "Yes"
Y <- as.factor(Y)
full <- lrm(NEC ~ TER + PREAFP + PREHCG + SQPOST + REDUC10,
            data = n544,
            x = TRUE,
            y = TRUE)
probs <- predict(full, type = "fitted.ind")
thresholds <- seq(0.005, 0.93, length = 50)
N <- length(thresholds)
TPR <- numeric(N) # true positive rates
FPR <- numeric(N) # false positive rates
for(i in 1:N){
        mod.pred <- rep("N", 544)
        mod.pred[probs > thresholds[i]] <- "Y"
        tb <- table(mod.pred, Y)[2:1,2:1] #confusion matrix
        TPR[i] <- tb[1,1] / (tb[1,1]+tb[2,1])
        FPR[i] <- tb[1,2] / (tb[1,2]+tb[2,2])
}
if(generate.file)
        jpeg(filename = "roc-example.jpeg", width = 640, height = 380)
op <- par(mar = c(4,4,1,1))
plot(x = FPR, y = TPR,
     type = "p", pch = 1, cex = 0.5,
     xlim = c(0,1), ylim = c(0,1),
     ylab = "True Positive Rate", xlab = "False Positive Rate")
lines(x = c(0,1), y = c(0,1), col = "gray")
text(y = 0.996, x = 0.756, label = "10%", cex = 0.8, pos = 1)
text(y = 0.906, x = 0.445, label = "30%", cex = 0.8, pos = 1)
if(generate.file) dev.off()
#
# Calculate an approximation to the
# area under the curve (AUC) using
# mid-point as the height of the
# approximating rectangles
#
FPR <- rev(FPR)
TPR <- rev(TPR)
rect.base <- (FPR[-1] - FPR[-length(FPR)])
AUC <- (sum(rect.base * TPR[-1]) + sum(rect.base * TPR[-length(TPR)]))/2
