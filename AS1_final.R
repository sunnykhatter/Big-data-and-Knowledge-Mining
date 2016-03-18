# Lakshay Khatter
# Big Data and Knowledge Mining
# Assignment 1

# Question 3: 4 Visualization techniques
#First read in the file.
sales <- read.csv("~/Downloads/sales.csv")

#split data into smaller tables
canada <- subset(sales, country=="Canada")
usa    <- subset(sales, country=="USA")

#Visualization #1: Pie Chart comparing overall sales distribution
canada_sum <- sum(as.numeric(c(canada$amount)))
usa_sum <- sum(as.numeric(c(usa$amount)))
slices <- c(canada_sum, usa_sum)
labels <- c("Canada","USA")
percentage <- round(slices/ sum(slices) * 100)
labels <- paste(labels, percentage)
labels <- paste(labels,"%",sep="") # add % to labels 
pie(c(canada_sum,usa_sum),labels, main="Sales distribution of Canada and USA")

#Visualization #2: Bar-Charts showing distrabution of products of USA and Canada
counts <- c(table(usa$product,canada$product))
barplot(counts, col=c("darkblue","red"),legend = rownames(counts), beside=TRUE,main = "USA(Blue) and Canada(Red) Product Count")

#Visualization #3: Average Sale size of Chairs
usa_chair <- subset(usa, product == "Chair")
histogram <- hist(as.numeric(as.matrix(usa_chair)[,length(usa_chair)]), breaks = 1000, main = "Order Quantity of Chair in US", xlab = "Chair Order Size")

#Vizualization #4: Compare Canadian Sales of Chair vs American Sales of Chair
boxplot((amount)~product, data=c(usa), xlab = "Product", ylab = "Amount in dollars($)", main = "Distribution of Sales of product in USA")
boxplot((amount)~product, data=c(canada), xlab = "Product", ylab = "Amount in dollars($)", main = "Distribution of Sales of product in Canada")


#Question 4:
#first put dates together to be recognized as months. 
sales$month <- factor(sales$month, level=month.abb)

#Create a cube to be used for analyzing revenue
cube<-tapply(sales$amount, sales[c("product","month","year","state")],FUN=function(x){return(sum(x))})

#1. Slice operation: compute the revenue for Laptop during January of 2013 in each state.
slice <- cube["Laptop","Jan","2013",]

#2. Dice operation: compute the revenue for the furniture products (Mattress and Chair) during the second quarter (April, May and June) of 2014 in each
#state.
dice <- cube[c("Mattress", "Chair"), c("Apr","May","Jun"),"2014",]

#3. Rollup operation: compute the annual revenue for each product and collapse the state and month dimensions.
rollup <- apply(cube, c("year","product"),FUN=function(x) {return(sum(x,na.rm = TRUE))} )

#4. Drilldown operation: compute the annual and monthly revenue for each product and collapse the state dimension
drilldown <- apply(cube, c("year", "month", "product"), FUN=function(x) {return(sum(x, na.rm=TRUE))})
