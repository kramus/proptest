#Function that calculate difference in proportions of MSM and IDU
#deaths for a specific year.
getwd()
#set() #change your directory to the one that contains the data
#Function:
library(plyr); library(dplyr)


data <- read.csv(".\\propt3.csv")
data$case <- factor(data$case, levels(data$case)[c(2,1)]) 
#one way of changing order of levels
levels(data$case)
data$type <- relevel(data$type, "msm") #another way of changing order of levels
levels(data$type)

prop <- function(x) {
        data <- filter(data, year == x)
        print(paste0("Proportions test of MSM vs IDU for year ", x))
        #you could also use cat("message", x) instead of paste0()
        ma <- with(data, xtabs(count~ type +case))
        ex <- chisq.test(ma)$expected
        ex <- as.numeric(ex)
        if (any(ex < 5)){
                fisher.test(ma)
        }
        else{
                chisq.test(ma)
        }
}


#sink(".\\output.txt")
print("Test of Proportions for MSM vs IDU")
prop("2004");prop("2005");prop("2006");prop("2007");prop("2008");prop("2009")
prop("2010");prop("2011");prop("2012");prop("2013");prop("2014")
#sink() #to restore outputs to console.


#When using lapply the print message appear at the beginning...
#lapply(2004:2014, FUN = prop)


a <- prop("2004")
b <- prop("2005")
c <- prop("2006")
d <- prop("2007")
e <- prop("2008")
f <- prop("2009")
g <- prop("2010")
h <- prop("2011")
i <- prop("2012")
j <- prop("2013")
k <- prop("2014")
lista <- list(a,b,c,d,e,f,g,h,i,j,k)
vec <- vector()
p_value <- sapply(1:11, function(x){
        if (x == 5 | x == 8) {
                vec[x] <- lista[[x]][[1]]
        }
        else {
                vec[x] <- lista[[x]][[3]]
        }
        })
p_value <- round(p_value, digits = 4)
Year <- 2004:2014

