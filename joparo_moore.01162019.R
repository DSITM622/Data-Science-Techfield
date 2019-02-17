# John Roberts
# 1/16/2019

getwd()



#load moore data
file <- read_csv("moore.csv", col_names = F)
View(file)

#Moore Data to use
moore <- file[2:3]
View(moore)

#change column name
colnames(moore) <- c("transistor", "year")

#remove brackets and contents within in year column
moore$year <- gsub("[[:punct:]][[:digit:]][[:digit:]][[:punct:]]","",moore$year)
moore$year <- gsub("[[:punct:]][[:digit:]][[:punct:]]","",moore$year)
View(moore)

#remove brackets, year and contents within in transistor column
moore$transistor <- gsub("[[:punct:]][[:digit:]][[:digit:]][[:punct:]]","",moore$transistor)
moore$transistor <- gsub("[[:punct:]][[:digit:]][[:punct:]]","",moore$transistor)
moore$transistor <- gsub(",","",moore$transistor)
moore$transistor <- gsub("cca ","",moore$transistor)
View(moore)


#remove Spaces in column year and transistor
moore$year <- gsub(" ","",moore$year)
moore$transistor <- gsub(" ","",moore$transistor)
View(moore)

#convert to numeric
moore$year <- as.numeric(moore$year)
moore$transistor <- as.numeric(moore$transistor)
View(moore)


# create plot
mooreplot1 = ggplot(data = moore, aes(x = year, y = transistor))
mooreplot1 +
  geom_point() +
  xlab("Year") +
  ylab("Transistors") 
  

# Transform
#Log of Y
YT <- log2(moore$transistor)

mooreplot2 = ggplot(data = moore, aes(x = year, y = YT))
mooreplot2 +
  geom_point() +
  xlab("Year") +
  ylab("Transistors") 


# Calculating Values
X <- moore$year
X


#denominator
d = mean(X**2) - mean(X)**2
d


w0 = (mean(YT)*mean(X**2) - mean(X)*mean(X*YT))/d 
w1 = (mean(X*YT)-mean(X)*mean(YT))/d  

yhat = w0 + w1*X


#plot regression line to data
mooreplot2 = ggplot(data = moore, aes(x = year, y = YT))
mooreplot2 +
  geom_point() +
  xlab("Year") +
  ylab("Transistors") +
  geom_abline(slope = w1, intercept = w0)


# R-squared = .9467453
r2 = 1 - sum((YT - yhat)**2)/sum((YT - mean(YT))**2)
r2



