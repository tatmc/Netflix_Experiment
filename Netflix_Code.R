
## PHASE 1
sample1 <- read.csv(file = "RESULTS_20814677_2021-08-12.csv", header = T)

sample1$Prev.Type <- NULL
sample1$Prev.Length[sample1$Prev.Length == 100] <- -1
sample1$Prev.Length[sample1$Prev.Length == 120] <- 1
sample1$Match.Score[sample1$Match.Score == 80] <- -1
sample1$Match.Score[sample1$Match.Score == 100] <- 1
sample1$Tile.Size[sample1$Tile.Size == 0.1] <- -1
sample1$Tile.Size[sample1$Tile.Size == 0.3] <- 1

Browse.Time <- sample1$Browse.Time
Prev.Length <- sample1$Prev.Length
Match.Score <- sample1$Match.Score
Tile.Size <- sample1$Tile.Size

df <- sample1[seq(8, nrow(sample1), 8), ]
mean(df$Browse.Time)

sample1$Browse.Time[sample1$Tile.Size == -1][sample1$Match.Score == -1][sample1$Prev.Length == -1]
mean(16.55575,16.77844,14.90119,15.38355,15.58028,16.93420,15.45613,16.72251,16.32373,16.07879,16.33069,16.01825,16.37346,15.30334,17.74285,15.28990,15.66055,15.05602,16.07044,16.43135,16.32730,13.58520,14.33444,16.67903
     ,16.24132,16.53319,16.48230,17.05493,16.74894,16.83529,16.39292,15.95473,16.14967,13.83534,14.77237,15.21075,17.03400,17.24185,11.52435,16.21400,16.93252,17.49357,16.20601,16.85741,17.14132,17.21987,17.01656,14.95014
     ,17.47164,16.70072,16.67754,15.37210,15.42770,17.15006,15.84102,17.40973,14.83878,14.26185,16.38037,17.39459,17.05989,14.21752,15.25899,14.83013,14.43302,15.51371,15.99882,16.26303,18.09054,15.10948,16.51361,16.49695
     ,15.55413,15.02372,17.75414,17.45635,15.78911,18.93173,16.80087,15.38722,16.32844,16.88591,16.56496,15.78696,16.01956,15.11880,15.37826,17.34539,16.02102,15.72317,15.22315,17.07085,16.75424,17.40173,18.51846,17.26251,15.45058,14.73798,15.74070,15.83446)

sample1$Browse.Time[sample1$Tile.Size == 1][sample1$Match.Score == -1][sample1$Prev.Length == -1]
mean(18.64409,15.99350,16.12495,15.01783,15.39136,16.56001,17.60525,16.76754,18.80830,14.92785,15.98626,16.62546,16.72645,16.81964,16.03493,14.81135,16.54093,15.39013,16.22483,17.29165,16.88256,16.16260,15.15762,17.88208,
     15.15477,16.35004,15.95633,15.57312,16.61890,17.33904,14.32574,17.59841,18.35456,17.62995,15.80562,14.71375,15.60734,15.54318,16.47284,16.95304,14.27416,14.93748,16.74655,15.19065,17.45968,17.01675,15.98002,16.44670,
     17.00476,16.22108,15.85057,15.92291,16.44389,16.39632,16.06195,16.64700,16.49973,15.91322,15.82103,18.49832,14.71278,18.12762,16.81524,14.90241,15.62735,14.01536,15.73507,14.80508,15.95838,15.94948,17.36034,15.04727,
     16.26147,15.37988,16.89362,15.86404,16.73476,15.07070,15.94964,16.52524,16.99505,16.82584,16.48432,16.34295,15.52122,15.38118,16.60854,15.84588,15.73504,15.76825,14.21726,15.54265,16.44690,16.71071,14.53994,15.10040,16.84840,17.28898,18.68902,14.81358)

sample1$Browse.Time[sample1$Tile.Size == 1][sample1$Match.Score == -1][sample1$Prev.Length == 1]


model_full <- lm(Browse.Time ~ Prev.Length * Match.Score * Tile.Size, data = sample1)
summary(model_full)
coef(summary(model_full))[,"Pr(>|t|)"]

# Prev.Length, Match Score and their interaction are significant

model_red <- lm(Browse.Time ~ Prev.Length + Match.Score + Prev.Length:Match.Score, data = sample1)
summary(model_red)
anova(model_red, model_full)


library(gplots)
par(mfrow=c(2,2)) 
plotmeans(formula = Browse.Time ~ Prev.Length, ylab = "Browsing Time", xlab = "Preview Length (sec)", 
          xaxt = "n", ylim = c(17,20), pch = 16, main = "Main Effect of Preview Length")
axis(side = 1, at = c(1,2), labels = c("100", "120"))
plotmeans(formula = Browse.Time ~ Match.Score, ylab = "Browsing Time", xlab = "Match Score (%)", 
          xaxt = "n", ylim = c(17,20), pch = 16, main = "Main Effect of Match Score")
axis(side = 1, at = c(1,2), labels = c("80", "100"))
plotmeans(formula = Browse.Time ~ Tile.Size, ylab = "Browsing Time", xlab = "Tile Size", 
          xaxt = "n", ylim = c(17,20), pch = 16, main = "Main Effect of Tile Size")
axis(side = 1, at = c(1,2), labels = c("0.1", "0.3"))
interaction.plot(Prev.Length, Match.Score, Browse.Time, ylab = "Browsing Time", xlab = "Preview Length (sec)", 
                 main = "Interraction Effect of Preview Length and Match Score", xaxt = "n", legend = F, ylim = c(15,20))
points(x = c(1,1), y = c(mean(sample1[sample1$Prev.Length==-1 & sample1$Match.Score==-1,]$Browse.Time), 
                         mean(sample1[sample1$Prev.Length==-1  & sample1$Match.Score==1,]$Browse.Time)), pch = 16)
points(x = c(2,2), y = c(mean(sample1[sample1$Prev.Length==1 & sample1$Match.Score==1,]$Browse.Time), 
                         mean(sample1[sample1$Prev.Length==1  & sample1$Match.Score==-1,]$Browse.Time)), pch = 16)
axis(side = 1, at = c(1,2), labels = c("100", "120"))
legend("bottomright", legend = c("Match Score", "100%", "80%"), lty = c(1,1,2), 
       col=c("white", "black", "black"), cex = 1.2)





## Phase 2

sample2 <- read.csv(file = "RESULTS_20814677_2021-08-13 copy.csv", header = T)
sample2$Prev.Type <- NULL
sample2$Tile.Size <- NULL

table(sample2$Prev.Length)


df <- sample2[seq(5, nrow(sample2), 5), ]
mean(df$Browse.Time)



# Function to create blues
blue_palette <- colorRampPalette(c(rgb(247,251,255,maxColorValue = 255), rgb(8,48,107,maxColorValue = 255)))

# Function for converting from natural units to coded units
convert.N.to.C <- function(U,UH,UL){
  x <- (U - (UH+UL)/2) / ((UH-UL)/2)
  return(x)
}

# Function for converting from coded units to natural units
convert.C.to.N <- function(x,UH,UL){
  U <- x*((UH-UL)/2) + (UH+UL)/2
  return(U)
}

# Function to create x and y grids for contour plots 
mesh <- function(x, y) { 
  Nx <- length(x)
  Ny <- length(y)
  list(
    x = matrix(nrow = Nx, ncol = Ny, data = x),
    y = matrix(nrow = Nx, ncol = Ny, data = y, byrow = TRUE)
  )
}



table(sample2$Prev.Length, sample2$Match.Score)

 
ph1 <- data.frame(y = sample2$Browse.Time,
                  x1 = convert.N.to.C(U = sample2$Prev.Length, UH = 120, UL = 100),
                  x2 = convert.N.to.C(U = sample2$Match.Score, UH = 100, UL = 80))
ph1$xPQ <- (ph1$x1^2 + ph1$x2^2)/2

## Check the average browsing time in each condition:
aggregate(ph1$y, by = list(x1 = ph1$x1, x2 = ph1$x2), FUN = mean)

## The difference in average browsing time in factorial conditions vs. the center 
## point condition
mean(ph1$y[ph1$xPQ != 0]) - mean(ph1$y[ph1$xPQ == 0])

## Check to see if that's significant
m <- lm(y~x1+x2+x1*x2+xPQ, data = ph1)
summary(m)

## Fit the first order model to determine the direction of the path of 
## steepest descent
m.fo <- lm(y~x1+x2, data = ph1)
beta0 <- coef(m.fo)[1]
beta1 <- coef(m.fo)[2]
beta2 <- coef(m.fo)[3]
grd <- mesh(x = seq(convert.N.to.C(U = 30, UH = 120, UL = 100), 
                    convert.N.to.C(U = 120, UH = 120, UL = 100), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 0, UH = 100, UL = 80), 
                    convert.N.to.C(U = 100, UH = 100, UL = 80), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.fo <- beta0 + beta1*x1 + beta2*x2

# plot
contour(x = seq(convert.N.to.C(U = 30, UH = 120, UL = 100), 
                convert.N.to.C(U = 120, UH = 120, UL = 100), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 0, UH = 100, UL = 80), 
                convert.N.to.C(U = 100, UH = 100, UL = 80), 
                length.out = 100), 
        z = eta.fo, xlab = "Preview Length (sec)", ylab = "Match Score (%)",
        nlevels = 15, col = blue_palette(15), labcex = 0.9, asp=1, main= "The Path of Steepest Descent")
abline(a = 0, b = beta2/beta1, lty = 2)
points(x = 0, y = 0, col = "red", pch = 16)

## Calculate the coordinates along this path that we will experiment at

# The gradient vector
g <- matrix(c(beta1, beta2), nrow = 1)

# We will take steps of size 5 seconds in preview length. In coded units this is
PL.step <- convert.N.to.C(U = 110 + 5, UH = 120, UL = 100)
lamda <- PL.step/abs(beta1)

## Step 0: The center point we've already observed
x.old <- matrix(0, nrow=1, ncol=2)
text(x = 0, y = 0+0.25, labels = "0")
step0 <- data.frame(Prev.Length = convert.C.to.N(x = 0, UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = 0, UH = 100, UL = 80))

## Step 1: 
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "1")
step1 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))


## Step 2: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "2")
step2 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 3: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "3")
step3 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 4: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "4")
step4 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 5: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "5")
step5 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 6: 
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "6")
step6 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))

## Step 7:
x.old <- x.new
x.new <- x.old - lamda*g
points(x = x.new[1,1], y = x.new[1,2], col = "red", pch = 16)
text(x = x.new[1,1], y = x.new[1,2]+0.25, labels = "7")
step7 <- data.frame(Prev.Length = convert.C.to.N(x = x.new[1,1], UH = 120, UL = 100), 
                    Match.Score = convert.C.to.N(x = x.new[1,2], UH = 100, UL = 80))



## The following is a list of the conditions along the path of steepest descent
pstd.cond <- data.frame(Step = 0:7, rbind(step0, step1, step2, step3, step4, step5, step6, step7))
pstd.cond


sample3 <- read.csv(file = "RESULTS_20814677_2021-08-13 copy 2.csv", header = T)
sample3$Prev.Type <- NULL
sample3$Tile.Size <- NULL
df.new <- sample2[seq(5, nrow(sample2), 5), ]
sample3 <- rbind(df.new, sample3)

## Calculate the average browsing time in each of these conditions and find the 
## condition that minimizes it
pstd.means <- aggregate(sample3$Browse.Time, 
                        by = list(Prev.Length = sample3$Prev.Length, 
                                  Match.Score = sample3$Match.Score), 
                        FUN = mean)
library(tidyverse)
pstd.means <- pstd.means %>% arrange(desc(Prev.Length))

plot(x = 0:7, y = pstd.means$x,
     type = "l", xlab = "Step Number", ylab = "Average Browsing Time (min)", main= "The Average Browsing Time in the First 7 Conditions")
points(x = 0:7, y = pstd.means$x,
       col = "red", pch = 16)
text(x = 0:7, y = pstd.means$x, labels = round(pstd.means$x,3), cex=0.9, pos=3)


pstd.cond[pstd.cond$Step == 4,]


sample4 <- read.csv(file = "RESULTS_20814677_2021-08-13 copy 4.csv", header = T)
sample4$Prev.Type <- NULL
sample4$Tile.Size <- NULL


ph_new <- data.frame(y = sample4$Browse.Time,
                    x1 = convert.N.to.C(U = sample4$Prev.Length, UH = 100, UL = 80),
                    x2 = convert.N.to.C(U = sample4$Match.Score, UH = 77, UL = 57))
ph_new$xPQ <- (ph_new$x1^2 + ph_new$x2^2)/2

## Check the average browsing time in each condition:
aggregate(ph_new$y, by = list(x1 = ph_new$x1, x2 = ph_new$x2), FUN = mean)

## The difference in average browsing time in factorial conditions vs. the center 
## point condition
mean(ph_new$y[ph_new$xPQ != 0]) - mean(ph_new$y[ph_new$xPQ == 0])

## Check to see if that's significant
m <- lm(y~x1+x2+x1*x2+xPQ, data = ph_new)
summary(m)

coef(summary(m))[,"Pr(>|t|)"]

## Yes, it is significant and so there is significant quadratic curvature in
## this region of the response surface. We should now commence phase 3 and 
## perform a respond surface design and fit a full second order model.



## Phase 3

sample5 <- read.csv(file = "RESULTS_20814677_2021-08-13 copy 5.csv", header = T)
sample5$Prev.Type <- NULL
sample5$Tile.Size <- NULL

sample5$Prev.Length[sample5$Prev.Length == 100] <- 1
sample5$Prev.Length[sample5$Prev.Length == 80] <- -1
sample5$Prev.Length[sample5$Prev.Length == 104] <- 1.4
sample5$Prev.Length[sample5$Prev.Length == 76] <- -1.4
sample5$Prev.Length[sample5$Prev.Length == 90] <- 0

sample5$Match.Score[sample5$Match.Score == 77] <- 1
sample5$Match.Score[sample5$Match.Score == 57] <- -1
sample5$Match.Score[sample5$Match.Score == 81] <- 1.4
sample5$Match.Score[sample5$Match.Score == 53] <- -1.4
sample5$Match.Score[sample5$Match.Score == 67] <- 0


condition <- data.frame(x1 = convert.C.to.N(x = c(-1,-1,1,1,0,1.4,-1.4,0,0), UH = 100, UL = 80), 
                        x2 = convert.C.to.N(x = c(-1,1,-1,1,0,0,0,1.4,-1.4), UH = 77, UL = 57))

## Calculate the booking rate in each condition
pi_hat <- aggregate(x = sample5$Browse.Time, by = list(condition.num = kronecker(1:9, rep(1, 100))), FUN = mean)
data.frame(Condition.Num = pi_hat$condition.num, 
           Prev.Length = condition$x1, 
           Match.Score = condition$x2,
           Browse.Time = pi_hat$x)

# We then fit the full 2nd-order response surface
model <- lm(Browse.Time ~ Prev.Length + Match.Score + Prev.Length*Match.Score + I(Prev.Length^2) + I(Match.Score^2), data = sample5)
summary(model)

## Let's visualize this surface:
beta0 <- coef(model)[1]
beta1 <- coef(model)[2]
beta2 <- coef(model)[3]
beta12 <- coef(model)[6]
beta11 <- coef(model)[4]
beta22 <- coef(model)[5]
grd <- mesh(x = seq(convert.N.to.C(U = 30, UH = 100, UL = 80), 
                    convert.N.to.C(U = 120, UH = 100, UL = 80), 
                    length.out = 100), 
            y = seq(convert.N.to.C(U = 0, UH = 77, UL = 57), 
                    convert.N.to.C(U = 100, UH = 77, UL = 57), 
                    length.out = 100))
x1 <- grd$x
x2 <- grd$y
eta.so <- beta0 + beta1*x1 + beta2*x2 + beta12*x1*x2 + beta11*x1^2 + beta22*x2^2

# 2D contour plot (coded units)
contour(x = seq(convert.N.to.C(U = 30, UH = 100, UL = 80), 
                convert.N.to.C(U = 120, UH = 100, UL = 80), 
                length.out = 100), 
        y = seq(convert.N.to.C(U = 0, UH = 77, UL = 57), 
                convert.N.to.C(U = 100, UH = 77, UL = 57), 
                length.out = 100), 
        z = eta.so, xlab = "x1", ylab = "x2",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

b <- matrix(c(beta1,beta2), ncol = 1)
B <- matrix(c(beta11, 0.5*beta12, 0.5*beta12, beta22), nrow = 2, ncol = 2)
x.s <- -0.5*solve(B) %*% b 
points(x = x.s[1], y = x.s[2], col = "red", pch = 16)

# The predicted book rate at this configuration is:
eta.s <- beta0 + 0.5*t(x.s) %*% b

# In natural units this optimum is located at
convert.C.to.N(x = x.s[1,1], UH = 100, UL = 80)
convert.C.to.N(x = x.s[2,1], UH = 77, UL = 57)

# Remake the contour plot but in natural units
contour(x = seq(30, 120, length.out = 100), 
        y = seq(0, 100, length.out = 100), 
        z = eta.so, xlab = "Discount Amount (%)", ylab = "Discount Duration (Days)",
        nlevels = 20, col = blue_palette(20), labcex = 0.9)

points(x = convert.C.to.N(x = x.s[1,1], UH = 100, UL = 80),
       y = convert.C.to.N(x = x.s[2,1], UH = 77, UL = 57), 
       col = "red", pch = 16)

points(x = 65, y = 80, pch = 16, col = "green")

## 95% prediction interval at this optimum:
n.data <- data.frame(Prev.Length=x.s[1,1], Match.Score=x.s[2,1])
pred <- predict(model, newdata = n.data, type = "response", se.fit = TRUE)
pred 
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))

## 95% prediction interval at convenient near-optimum:
n.data <- data.frame(x1=0, x2=-1)
pred <- predict(model, n.data, type = "response", se.fit = TRUE)
pred
print(paste("Prediction: ", pred$fit, sep = ""))
print(paste("95% Prediction interval: (", pred$fit-qnorm(0.975)*pred$se.fit, ",", pred$fit+qnorm(0.975)*pred$se.fit, ")", sep = ""))





