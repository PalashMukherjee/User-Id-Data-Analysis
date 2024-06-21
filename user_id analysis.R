#this contains all the code that i did to obtain the plots
library(dplyr)
library(readxl)
library(ggplot2)
Active_Users <- read_excel("C:/Users/Palash/Desktop/interview prep/Active Users.xls")
View(Active_Users)
# 1. calculating weekly total users
week.names <- c()
weekly.total.users <- c()
for (i in colnames(Active_Users)){
  week.names <- append(week.names,i)
  weekly.total.users <- append(weekly.total.users,sum(!is.na(Active_Users[i])))
}
print(week.names)
print(weekly.total.users)
df1 <- data.frame(week.names,weekly.total.users)
# Plotting the data
plot(df1$weekly.total.users, type = "o", col = "blue", pch = 19, lwd = 2,
     xlab = "Week", ylab = "Total Users",
     main = "Weekly Total Users")
# Adding points and lines
lines(df1$weekly.total.users, col = "blue", type = "o", pch = 19, lwd = 2)
# Adding grid lines
grid()
# Adding a legend
legend("topleft", legend = "Total Users", col = "blue", lty = 1, lwd = 2, bty = "n")

#-----------------------------------------------------------------------------
# assuming 1 month as 4 weeks
## calculating total monthly users 
split(week.names,rep(1:14,each=4))
iterator = 1
for (c in split(week.names,rep(1:14,each=4))){
  assign(sprintf("month_%d",iterator),Active_Users[c])
  iterator <- iterator + 1
}


new.monthly.users <- function(dataframe){
  monthly.active.users <- c()
  for (x in colnames(dataframe)){
    monthly.active.users <- append(monthly.active.users,na.omit(dataframe[x]))
  }
  return (monthly.active.users)
}

length(unique(unlist(new.monthly.users(month_1))))
length(unique(unlist(new.monthly.users(month_2))))
length(unique(unlist(new.monthly.users(month_3))))
length(unique(unlist(new.monthly.users(month_4))))
length(unique(unlist(new.monthly.users(month_5))))
length(unique(unlist(new.monthly.users(month_6))))
length(unique(unlist(new.monthly.users(month_7))))
length(unique(unlist(new.monthly.users(month_8))))
length(unique(unlist(new.monthly.users(month_9))))
length(unique(unlist(new.monthly.users(month_10))))
length(unique(unlist(new.monthly.users(month_11))))
length(unique(unlist(new.monthly.users(month_12))))
length(unique(unlist(new.monthly.users(month_13))))
length(unique(unlist(new.monthly.users(month_14))))


#___________________________________________________________________
total.unique.users.monthly <- numeric()
# Iterate over months 1 to 14
for (i in 1:14) {
  function_call <- paste0("new.monthly.users(month_", i, ")")
  unique_length <- length(unique(unlist(eval(parse(text = function_call)))))
  total.unique.users.monthly <- c(total.unique.users.monthly, unique_length)
}
print(total.unique.users.monthly)

# Plotting the data
plot(total.unique.users.monthly, type = "o", col = "blue", pch = 19, lwd = 2,
     xlab = "Month", ylab = "Total Users",
     main = "Monthly Total Users")
# Adding points and lines
lines(total.unique.users.monthly, col = "blue", type = "o", pch = 19, lwd = 2)
# Adding grid lines
grid()
# Adding a legend
legend("topleft", legend = "Total Users", col = "blue", lty = 1, lwd = 2, bty = "n")

#--------------------------------------------------------------------------------------------------
#calculating new users every week
new.user.w1 <- length(na.omit(Active_Users$w1))
new.user.w2 <- length(setdiff(na.omit(Active_Users$w2),na.omit(Active_Users$w1)))
new.user.w3 <- length(setdiff(na.omit(Active_Users$w3),na.omit(c(Active_Users$w1,Active_Users$w2))))
new.user.w4 <- length(setdiff(na.omit(Active_Users$w4),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3))))
new.user.w5 <- length(setdiff(na.omit(Active_Users$w5),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4))))
new.user.w6 <- length(setdiff(na.omit(Active_Users$w6),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5))))
new.user.w7 <- length(setdiff(na.omit(Active_Users$w7),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6))))
new.user.w8 <- length(setdiff(na.omit(Active_Users$w8),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7))))
new.user.w9 <- length(setdiff(na.omit(Active_Users$w9),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8))))
new.user.w10 <- length(setdiff(na.omit(Active_Users$w10),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9))))
new.user.w11 <- length(setdiff(na.omit(Active_Users$w11),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10))))
new.user.w12 <- length(setdiff(na.omit(Active_Users$w12),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11))))
new.user.w13 <- length(setdiff(na.omit(Active_Users$w13),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12))))
new.user.w14 <- length(setdiff(na.omit(Active_Users$w14),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13))))
new.user.w15 <- length(setdiff(na.omit(Active_Users$w15),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14))))
new.user.w16 <- length(setdiff(na.omit(Active_Users$w16),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15))))
new.user.w17 <- length(setdiff(na.omit(Active_Users$w17),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16))))
new.user.w18 <- length(setdiff(na.omit(Active_Users$w18),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17))))
new.user.w19 <- length(setdiff(na.omit(Active_Users$w19),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18))))
new.user.w20 <- length(setdiff(na.omit(Active_Users$w20),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19))))
new.user.w21 <- length(setdiff(na.omit(Active_Users$w21),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20))))
new.user.w22 <- length(setdiff(na.omit(Active_Users$w22),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21))))
new.user.w23 <- length(setdiff(na.omit(Active_Users$w23),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22))))
new.user.w24 <- length(setdiff(na.omit(Active_Users$w24),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23))))
new.user.w25 <- length(setdiff(na.omit(Active_Users$w25),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24))))
new.user.w26 <- length(setdiff(na.omit(Active_Users$w26),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25))))
new.user.w27 <- length(setdiff(na.omit(Active_Users$w27),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26))))
new.user.w28 <- length(setdiff(na.omit(Active_Users$w28),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27))))
new.user.w29 <- length(setdiff(na.omit(Active_Users$w29),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28))))
new.user.w30 <- length(setdiff(na.omit(Active_Users$w30),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29))))
new.user.w31 <- length(setdiff(na.omit(Active_Users$w31),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30))))
new.user.w32 <- length(setdiff(na.omit(Active_Users$w32),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31))))
new.user.w33 <- length(setdiff(na.omit(Active_Users$w33),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32))))
new.user.w34 <- length(setdiff(na.omit(Active_Users$w34),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33))))
new.user.w35 <- length(setdiff(na.omit(Active_Users$w35),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34))))
new.user.w36 <- length(setdiff(na.omit(Active_Users$w36),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35))))
new.user.w37 <- length(setdiff(na.omit(Active_Users$w37),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36))))
new.user.w38 <- length(setdiff(na.omit(Active_Users$w38),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37))))
new.user.w39 <- length(setdiff(na.omit(Active_Users$w39),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38))))
new.user.w40 <- length(setdiff(na.omit(Active_Users$w40),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39))))
new.user.w41 <- length(setdiff(na.omit(Active_Users$w41),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40))))
new.user.w42 <- length(setdiff(na.omit(Active_Users$w42),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41))))
new.user.w43 <- length(setdiff(na.omit(Active_Users$w43),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42))))
new.user.w44 <- length(setdiff(na.omit(Active_Users$w44),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43))))
new.user.w45 <- length(setdiff(na.omit(Active_Users$w45),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44))))
new.user.w46 <- length(setdiff(na.omit(Active_Users$w46),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45))))
new.user.w47 <- length(setdiff(na.omit(Active_Users$w47),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46))))
new.user.w48 <- length(setdiff(na.omit(Active_Users$w48),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47))))
new.user.w49 <- length(setdiff(na.omit(Active_Users$w49),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48))))
new.user.w50 <- length(setdiff(na.omit(Active_Users$w50),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49))))
new.user.w51 <- length(setdiff(na.omit(Active_Users$w51),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49,Active_Users$w50))))
new.user.w52 <- length(setdiff(na.omit(Active_Users$w52),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49,Active_Users$w50,Active_Users$w51))))
new.user.w53 <- length(setdiff(na.omit(Active_Users$w53),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49,Active_Users$w50,Active_Users$w51,Active_Users$w52))))
new.user.w54 <- length(setdiff(na.omit(Active_Users$w54),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49,Active_Users$w50,Active_Users$w51,Active_Users$w52,Active_Users$w53))))
new.user.w55 <- length(setdiff(na.omit(Active_Users$w55),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49,Active_Users$w50,Active_Users$w51,Active_Users$w52,Active_Users$w53,Active_Users$w54))))
new.user.w56 <- length(setdiff(na.omit(Active_Users$w56),na.omit(c(Active_Users$w1,Active_Users$w2,Active_Users$w3,Active_Users$w4,Active_Users$w5,Active_Users$w6,Active_Users$w7,Active_Users$w8,Active_Users$w9,Active_Users$w10,Active_Users$w11,Active_Users$w12,Active_Users$w13,Active_Users$w14,Active_Users$w15,Active_Users$w16,Active_Users$w17,Active_Users$w18,Active_Users$w19,Active_Users$w20,Active_Users$w21,Active_Users$w22,Active_Users$w23,Active_Users$w24,Active_Users$w25,Active_Users$w26,Active_Users$w27,Active_Users$w28,Active_Users$w29,Active_Users$w30,Active_Users$w31,Active_Users$w32,Active_Users$w33,Active_Users$w34,Active_Users$w35,Active_Users$w36,Active_Users$w37,Active_Users$w38,Active_Users$w39,Active_Users$w40,Active_Users$w41,Active_Users$w42,Active_Users$w43,Active_Users$w44,Active_Users$w45,Active_Users$w46,Active_Users$w47,Active_Users$w48,Active_Users$w49,Active_Users$w50,Active_Users$w51,Active_Users$w52,Active_Users$w53,Active_Users$w54,Active_Users$w55))))


new.user.vector <- c()
for (q in week.names){
  new.user.vector <- append(new.user.vector,get(paste0("new.user.",q)))
}

# Create the line plot
plot(new.user.vector, type = "o", col = "blue", pch = 19, lwd = 2,
     xlab = "Week", ylab = "New Users", main = "Weekly New Users", xaxt = 'n')
# Add x-axis labels
axis(1, at = 1:length(week.names), labels = week.names, las = 2)
# Adding grid lines for better readability
grid()
# Adding a legend
legend("topright", legend = "New User", col = "blue", lty = 1, lwd = 2, pch = 19, bty = "n")


#---------------------------------------------------------------------------------------------
#churn users

#for(p in 2:56){
#  print(length(setdiff(c(na.omit(Active_Users[p-1])),unique(na.omit(unlist(c(select(Active_Users,c(p:56)))))))))
#}

stacked.df <- na.omit(stack(select(Active_Users,c(2:56))))
dim(stacked.df)
View(stacked.df)
length(setdiff(na.omit(Active_Users[["w1"]]),na.omit(stack(select(Active_Users,c(2:56))))$values))



churn.customer <- c()
for (p in 2:56){
  column.name <- paste0("w",p-1)
  print(column.name)
  print(length(setdiff(na.omit(Active_Users[[column.name]]),na.omit(stack(select(Active_Users,c(p:56))))$values)))
  churn.customer <- append(churn.customer,length(setdiff(na.omit(Active_Users[[column.name]]),na.omit(stack(select(Active_Users,c(p:56))))$values)))
  print("------------------------------------")
}

plot(churn.customer, type = "o", col = "blue", pch = 19, lwd = 2,
     xaxt = "n", xlab = "Week", ylab = "Churn Customers",
     main = "Weekly Churn Customers")
lines(churn.customer, col = "blue", type = "o", pch = 19, lwd = 2)
grid()
axis(1, at = 1:55, labels = week.names[-56], las = 2, cex.axis = 0.7)
legend("topright", legend = "Churn Customers", col = "blue", lty = 1, lwd = 2, bty = "n")



#------------------------------------------------------------------------------------------
#considering resurrected customers that is customers who have logged in 
#after 30 days or after a month

stacked.df1 <- na.omit(stack(select(Active_Users,c(2,3,4,5))))
#customers in week1 but not in week2, 3, 4
#setdiff(na.omit(Active_Users$w1),stacked.df1$values)
#common people who were in week1 and are resurrected in week5 after 30 days
length(intersect(setdiff(na.omit(Active_Users$w1),stacked.df1$values),Active_Users$w6))


stacked.df2 <- na.omit(stack(select(Active_Users,c(3,4,5,6))))
length(intersect(setdiff(na.omit(Active_Users$w2),stacked.df2$values),Active_Users$w7))

stacked.df3 <- na.omit(stack(select(Active_Users,c(4,5,6,7))))
length(intersect(setdiff(na.omit(Active_Users$w3),stacked.df3$values),Active_Users$w8))

stacked.df4 <- na.omit(stack(select(Active_Users,c(5,6,7,8))))
length(intersect(setdiff(na.omit(Active_Users$w4),stacked.df4$values),Active_Users$w9))

stacked.df5 <- na.omit(stack(select(Active_Users,c(6,7,8,9))))
length(intersect(setdiff(na.omit(Active_Users$w5),stacked.df5$values),Active_Users$w10))

stacked.df6 <- na.omit(stack(select(Active_Users,c(7,8,9,10))))
length(intersect(setdiff(na.omit(Active_Users$w6),stacked.df6$values),Active_Users$w11))

stacked.df7 <- na.omit(stack(select(Active_Users,c(8,9,10,11))))
length(intersect(setdiff(na.omit(Active_Users$w7),stacked.df7$values),Active_Users$w12))

stacked.df8 <- na.omit(stack(select(Active_Users,c(9,10,11,12))))
length(intersect(setdiff(na.omit(Active_Users$w8),stacked.df8$values),Active_Users$w13))

stacked.df9 <- na.omit(stack(select(Active_Users,c(10,11,12,13))))
length(intersect(setdiff(na.omit(Active_Users$w9),stacked.df9$values),Active_Users$w14))

stacked.df10 <- na.omit(stack(select(Active_Users,c(11,12,13,14))))
length(intersect(setdiff(na.omit(Active_Users$w10),stacked.df10$values),Active_Users$w15))

stacked.df11 <- na.omit(stack(select(Active_Users,c(12,13,14,15))))
length(intersect(setdiff(na.omit(Active_Users$w11),stacked.df11$values),Active_Users$w16))

stacked.df12 <- na.omit(stack(select(Active_Users,c(13,14,15,16))))
length(intersect(setdiff(na.omit(Active_Users$w12),stacked.df12$values),Active_Users$w17))

stacked.df13 <- na.omit(stack(select(Active_Users,c(14,15,16,17))))
length(intersect(setdiff(na.omit(Active_Users$w13),stacked.df13$values),Active_Users$w18))

stacked.df14 <- na.omit(stack(select(Active_Users,c(15,16,17,18))))
length(intersect(setdiff(na.omit(Active_Users$w14),stacked.df14$values),Active_Users$w19))

stacked.df15 <- na.omit(stack(select(Active_Users,c(16,17,18,19))))
length(intersect(setdiff(na.omit(Active_Users$w15),stacked.df15$values),Active_Users$w20))

stacked.df16 <- na.omit(stack(select(Active_Users,c(17,18,19,20))))
length(intersect(setdiff(na.omit(Active_Users$w16),stacked.df16$values),Active_Users$w21))

stacked.df17 <- na.omit(stack(select(Active_Users,c(18,19,20,21))))
length(intersect(setdiff(na.omit(Active_Users$w17),stacked.df17$values),Active_Users$w22))

stacked.df18 <- na.omit(stack(select(Active_Users,c(19,20,21,22))))
length(intersect(setdiff(na.omit(Active_Users$w18),stacked.df18$values),Active_Users$w23))

stacked.df19 <- na.omit(stack(select(Active_Users,c(20,21,22,23))))
length(intersect(setdiff(na.omit(Active_Users$w19),stacked.df19$values),Active_Users$w24))

stacked.df20 <- na.omit(stack(select(Active_Users,c(21,22,23,24))))
length(intersect(setdiff(na.omit(Active_Users$w20),stacked.df20$values),Active_Users$w25))

stacked.df21 <- na.omit(stack(select(Active_Users,c(22,23,24,25))))
length(intersect(setdiff(na.omit(Active_Users$w21),stacked.df21$values),Active_Users$w26))

stacked.df22 <- na.omit(stack(select(Active_Users,c(23,24,25,26))))
length(intersect(setdiff(na.omit(Active_Users$w22),stacked.df22$values),Active_Users$w27))

stacked.df23 <- na.omit(stack(select(Active_Users,c(24,25,26,27))))
length(intersect(setdiff(na.omit(Active_Users$w23),stacked.df23$values),Active_Users$w28))

stacked.df24 <- na.omit(stack(select(Active_Users,c(25,26,27,28))))
length(intersect(setdiff(na.omit(Active_Users$w24),stacked.df24$values),Active_Users$w29))

stacked.df24 <- na.omit(stack(select(Active_Users,c(25,26,27,28))))
length(intersect(setdiff(na.omit(Active_Users$w24),stacked.df24$values),Active_Users$w29))

stacked.df25 <- na.omit(stack(select(Active_Users,c(26,27,28,29))))
length(intersect(setdiff(na.omit(Active_Users$w25),stacked.df25$values),Active_Users$w30))

stacked.df26 <- na.omit(stack(select(Active_Users,c(27,28,29,30))))
length(intersect(setdiff(na.omit(Active_Users$w26),stacked.df26$values),Active_Users$w31))

stacked.df27 <- na.omit(stack(select(Active_Users,c(28,29,30,31))))
length(intersect(setdiff(na.omit(Active_Users$w27),stacked.df27$values),Active_Users$w32))

stacked.df28 <- na.omit(stack(select(Active_Users,c(29,30,31,32))))
length(intersect(setdiff(na.omit(Active_Users$w28),stacked.df28$values),Active_Users$w33))

stacked.df29 <- na.omit(stack(select(Active_Users,c(30,31,32,33))))
length(intersect(setdiff(na.omit(Active_Users$w29),stacked.df29$values),Active_Users$w34))

stacked.df30 <- na.omit(stack(select(Active_Users,c(31,32,33,34))))
length(intersect(setdiff(na.omit(Active_Users$w30),stacked.df30$values),Active_Users$w35))

stacked.df31 <- na.omit(stack(select(Active_Users,c(32,33,34,35))))
length(intersect(setdiff(na.omit(Active_Users$w31),stacked.df31$values),Active_Users$w36))

stacked.df32 <- na.omit(stack(select(Active_Users,c(33,34,35,36))))
length(intersect(setdiff(na.omit(Active_Users$w32),stacked.df32$values),Active_Users$w37))

stacked.df33 <- na.omit(stack(select(Active_Users,c(34,35,36,37))))
length(intersect(setdiff(na.omit(Active_Users$w33),stacked.df33$values),Active_Users$w38))

stacked.df34 <- na.omit(stack(select(Active_Users,c(35,36,37,38))))
length(intersect(setdiff(na.omit(Active_Users$w34),stacked.df34$values),Active_Users$w39))

stacked.df35 <- na.omit(stack(select(Active_Users,c(36,37,38,39))))
length(intersect(setdiff(na.omit(Active_Users$w35),stacked.df35$values),Active_Users$w40))

stacked.df36 <- na.omit(stack(select(Active_Users,c(37,38,39,40))))
length(intersect(setdiff(na.omit(Active_Users$w36),stacked.df36$values),Active_Users$w41))

stacked.df37 <- na.omit(stack(select(Active_Users,c(38,39,40,41))))
length(intersect(setdiff(na.omit(Active_Users$w37),stacked.df37$values),Active_Users$w42))

stacked.df38 <- na.omit(stack(select(Active_Users,c(39,40,41,42))))
length(intersect(setdiff(na.omit(Active_Users$w38),stacked.df38$values),Active_Users$w43))

stacked.df39 <- na.omit(stack(select(Active_Users,c(40,41,42,43))))
length(intersect(setdiff(na.omit(Active_Users$w39),stacked.df39$values),Active_Users$w44))

stacked.df40 <- na.omit(stack(select(Active_Users,c(41,42,43,44))))
length(intersect(setdiff(na.omit(Active_Users$w40),stacked.df40$values),Active_Users$w45))

stacked.df41 <- na.omit(stack(select(Active_Users,c(42,43,44,45))))
length(intersect(setdiff(na.omit(Active_Users$w41),stacked.df41$values),Active_Users$w46))

stacked.df42 <- na.omit(stack(select(Active_Users,c(43,44,45,46))))
length(intersect(setdiff(na.omit(Active_Users$w42),stacked.df42$values),Active_Users$w47))

stacked.df43 <- na.omit(stack(select(Active_Users,c(44,45,46,47))))
length(intersect(setdiff(na.omit(Active_Users$w43),stacked.df43$values),Active_Users$w48))

stacked.df44 <- na.omit(stack(select(Active_Users,c(45,46,47,48))))
length(intersect(setdiff(na.omit(Active_Users$w44),stacked.df44$values),Active_Users$w49))

stacked.df45 <- na.omit(stack(select(Active_Users,c(46,47,48,49))))
length(intersect(setdiff(na.omit(Active_Users$w45),stacked.df45$values),Active_Users$w50))

stacked.df46 <- na.omit(stack(select(Active_Users,c(47,48,49,50))))
length(intersect(setdiff(na.omit(Active_Users$w46),stacked.df46$values),Active_Users$w51))

stacked.df47 <- na.omit(stack(select(Active_Users,c(48,49,50,51))))
length(intersect(setdiff(na.omit(Active_Users$w47),stacked.df47$values),Active_Users$w52))

stacked.df48 <- na.omit(stack(select(Active_Users,c(49,50,51,52))))
length(intersect(setdiff(na.omit(Active_Users$w48),stacked.df48$values),Active_Users$w53))

stacked.df49 <- na.omit(stack(select(Active_Users,c(50,51,52,53))))
length(intersect(setdiff(na.omit(Active_Users$w49),stacked.df49$values),Active_Users$w54))

stacked.df50 <- na.omit(stack(select(Active_Users,c(51,52,53,54))))
length(intersect(setdiff(na.omit(Active_Users$w50),stacked.df50$values),Active_Users$w55))

stacked.df51 <- na.omit(stack(select(Active_Users,c(52,53,54,55))))
length(intersect(setdiff(na.omit(Active_Users$w51),stacked.df51$values),Active_Users$w56))

# Plotting
data <- c(21, 7, 5, 14, 16, 13, 11, 16, 19, 20, 16, 21, 21, 26, 8, 11, 20, 24, 20, 19, 14, 20, 16, 13, 13, 11, 11, 21, 17, 22, 12, 12, 19, 11, 24, 11, 27, 12, 13, 16, 14, 44, 34, 26, 22, 17, 22, 29, 26, 44, 29, 31)
weeks <- seq(5, 56)
plot(weeks, data, type="o", col="blue", xlab="Week", ylab="Value", main="Resurected users (span of 30days approx)")
grid()



#------------------------------------------------------------------------------------
#retained user
#timer period for considering user retention
#retention rate month over month

# Function to find common values among all columns of dataframe
find_common_values <- function(df) {
  Reduce(intersect, lapply(df, unique))
}

#number of people who were active in the month1 and month2
length(na.omit(find_common_values(cbind(month_1,month_2))))

#numnber of customer who were active in the month1
length(na.omit(find_common_values(month_1)))

r1 <- length(na.omit(find_common_values(cbind(month_1,month_2))))/length(na.omit(find_common_values(month_1)))
#--------------------------------------------------------------

#number of people who were active in the month2 and month3
length(na.omit(find_common_values(cbind(month_2,month_3))))

#numnber of customer who were active in the month2
length(na.omit(find_common_values(month_2)))
r2 <- length(na.omit(find_common_values(cbind(month_2,month_3))))/length(na.omit(find_common_values(month_2)))
#---------------------------------------------------------------
#number of people who were active in the month3 and month4
length(na.omit(find_common_values(cbind(month_3,month_4))))

#numnber of customer who were active in the month3
length(na.omit(find_common_values(month_3)))
r3 <- length(na.omit(find_common_values(cbind(month_3,month_4))))/length(na.omit(find_common_values(month_3)))


#-------------------------------------------------------------------
#number of people who were active in the month4 and month5
length(na.omit(find_common_values(cbind(month_4,month_5))))

#numnber of customer who were active in the month4
length(na.omit(find_common_values(month_4)))

r4<- length(na.omit(find_common_values(cbind(month_4,month_5))))/length(na.omit(find_common_values(month_4)))

#--------------------------------------------------------------------
#number of people who were active in the month5 and month6
length(na.omit(find_common_values(cbind(month_5,month_6))))

#numnber of customer who were active in the month5
length(na.omit(find_common_values(month_5)))
r5 <- length(na.omit(find_common_values(cbind(month_5,month_6))))/length(na.omit(find_common_values(month_5)))

#----------------------------------------------------------------------
#number of people who were active in the month6 and month7
length(na.omit(find_common_values(cbind(month_6,month_7))))

#numnber of customer who were active in the month6
length(na.omit(find_common_values(month_6)))
r6 <- length(na.omit(find_common_values(cbind(month_6,month_7))))/length(na.omit(find_common_values(month_6)))

#-----------------------------------------------------------------------
#number of people who were active in the month6 and month7
length(na.omit(find_common_values(cbind(month_6,month_7))))

#numnber of customer who were active in the month6
length(na.omit(find_common_values(month_6)))
r7<-length(na.omit(find_common_values(cbind(month_6,month_7))))/length(na.omit(find_common_values(month_6)))

#------------------------------------------------------------------------
#number of people who were active in the month7 and month8
length(na.omit(find_common_values(cbind(month_7,month_8))))

#numnber of customer who were active in the month7
length(na.omit(find_common_values(month_7)))
r8 <- length(na.omit(find_common_values(cbind(month_7,month_8))))/length(na.omit(find_common_values(month_7)))

#-------------------------------------------------------------------------
#number of people who were active in the month8 and month9
length(na.omit(find_common_values(cbind(month_8,month_9))))

#numnber of customer who were active in the month8
length(na.omit(find_common_values(month_8)))
r9 <- length(na.omit(find_common_values(cbind(month_8,month_9))))/length(na.omit(find_common_values(month_8)))

#--------------------------------------------------------------------------
#number of people who were active in the month9 and month10
length(na.omit(find_common_values(cbind(month_9,month_10))))

#numnber of customer who were active in the month9
length(na.omit(find_common_values(month_9)))
r10<-length(na.omit(find_common_values(cbind(month_9,month_10))))/length(na.omit(find_common_values(month_9)))

#--------------------------------------------------------------------------
#number of people who were active in the month9 and month10
length(na.omit(find_common_values(cbind(month_9,month_10))))

#numnber of customer who were active in the month9
length(na.omit(find_common_values(month_9)))
r11 <- length(na.omit(find_common_values(cbind(month_9,month_10))))/length(na.omit(find_common_values(month_9)))

#---------------------------------------------------------------------------
#number of people who were active in the month10 and month11
length(na.omit(find_common_values(cbind(month_10,month_11))))

#numnber of customer who were active in the month10
length(na.omit(find_common_values(month_10)))
r12 <- length(na.omit(find_common_values(cbind(month_10,month_11))))/length(na.omit(find_common_values(month_10)))

#---------------------------------------------------------------------------
#number of people who were active in the month11 and month12
length(na.omit(find_common_values(cbind(month_11,month_12))))

#numnber of customer who were active in the month11
length(na.omit(find_common_values(month_11)))
r13 <- length(na.omit(find_common_values(cbind(month_11,month_12))))/length(na.omit(find_common_values(month_11)))

#----------------------------------------------------------------------------
#number of people who were active in the month11 and month12
length(na.omit(find_common_values(cbind(month_11,month_12))))

#numnber of customer who were active in the month11
length(na.omit(find_common_values(month_11)))
r14 <- length(na.omit(find_common_values(cbind(month_11,month_12))))/length(na.omit(find_common_values(month_11)))

#-----------------------------------------------------------------------------
#number of people who were active in the month12 and month13
length(na.omit(find_common_values(cbind(month_12,month_13))))

#numnber of customer who were active in the month12
length(na.omit(find_common_values(month_12)))
r15 <- length(na.omit(find_common_values(cbind(month_12,month_13))))/length(na.omit(find_common_values(month_12)))


#-----------------------------------------------------------------------
#number of people who were active in the month13 and month14
length(na.omit(find_common_values(cbind(month_13,month_14))))

#numnber of customer who were active in the month13
length(na.omit(find_common_values(month_13)))
r16 <- length(na.omit(find_common_values(cbind(month_13,month_14))))/length(na.omit(find_common_values(month_13)))


retention.rate.vector <- c()
for (v in 1:16){
  retention.rate.vector <- append(retention.rate.vector,get(paste0("r",v)))
}
plot(retention.rate.vector, type="o", col="blue", xlab="Month", ylab="retention rate", main="Monthwise Retention Rate")
grid()




#---------------------------------------------------------------------------------------------
#quick ratio
#new user vector  ->   new.user.vector
#churned user    ->   churn.customer
#resurrected user   ->   data
length(data)
#considering retention period to be 1 month

#deleting the last value in each vector
#and first 4 weeks data because retention period was 4weeks
length(new.user.vector[-c(1,2,3,4,56)])
length(churn.customer[-c(1,2,3,4)])
length(data[-52])

#(new + ressurected)/churned
quick.ratio <- (new.user.vector[-c(1,2,3,4,56)] + data[-52])/churn.customer[-c(1,2,3,4)]
week.names[-c(1,2,3,4,56)]

data.quick.ratio <- data.frame(week.names[-c(1,2,3,4,56)],quick.ratio)
x_values <- 5:55
length(x_values)
library(ggplot2)

# Plot the data
plot(x_values,data.quick.ratio$quick.ratio, type = "o", col = "blue", pch = 16,
     xlab = "Week", ylab = "Quick Ratio", main = "Quick Ratio Over Time",
     xlim = c(1,56))  # xaxt="n" to customize x-axis, las=2 to rotate labels

# Add custom x-axis labels
axis(1, at = 1:48, labels = data$Week, las = 2)

# Add grid lines
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")


#-----------------------------------------------------------------------------
#new user vs churned user
# Create the plot with the first vector
c(churn.customer,NA)
length(new.user.vector)
c(1,2,3,4,weeks)
weeks <- 1:56
# Create the plot for new users
plot(weeks, new.user.vector, type = "o", col = "blue", pch = 16,
     xlab = "Weeks", ylab = "Number of Users", main = "New Users Vs Churned Users",
     xlim = c(0, max(weeks) + 1), ylim = c(0, max(new.user.vector, churn.customer) + 5),
     xaxt = "n")  # xaxt="n" to customize x-axis

# Add line plot for churned users (with NA for the first point to align with new users)
lines(weeks, c(churn.customer,NA), type = "o", col = "red", pch = 18)

# Add legend
legend("topright", legend = c("New User", "Churned User"),
       col = c("blue", "red"), pch = c(16, 18), lty = c(1, 1))

# Add grid lines
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")

# Customizing x-axis labels
axis(1, at = weeks, labels = paste("W", weeks), tick = TRUE, cex.axis = 0.8)

# Adding a title for x-axis
mtext("Weeks", side = 1, line = 3, cex.lab = 1.2)

# Adding a title for y-axis
mtext("Number of Users", side = 2, line = 3, cex.lab = 1.2)

