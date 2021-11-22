# Question 1

# Snippet 1

x1 <- 1/3 ; x2 <- 1/4

if ((x1-x2) == 1/12){
  print("Subtraction is correct")
  }else{
    print("Subtraction is wrong")
  }

# Improvement 1
x1 <- 1/3 ; x2 <- 1/4

if ( signif(x1-x2, digits = 6) == signif(1/12, digits = 6)){ 
  # Setting or not digits does not make any major difference.
  print("Subtraction is correct")
  }else{
    print("Subtraction is wrong")
  }

# Improvement 2

x1 = 1/3 ; x2 = 1/4

if(all.equal(x1 - x2, 1/12)){
  print("Subtraction is correct")
} else{
  print("Subtraction is wrong")
}

## Snippet 2

x1 <- 1
x2 <- 1/2
if (x1-x2 == 1/2){
  print("Subtraction is correct") 
  }else{
    print("Subtraction is wrong")
  }


################################################################################

# Question 2

my_derivative <- function(x){
  e <- 10^(-15)
  d <- ((x + e) - x)/e
  return(d)
}

my_derivative(1) # returns 1.110223 

my_derivative(100000) # returns 0

# Question 3

myvar <- function(x){
  n <- length(x)
  s1 <- sum(x^2)
  s2 <- (1/n)*((sum(x))^2)
  variance <- (1/(n-1))*(s1-s2)
  return(variance)
}

x <- rnorm( n = 10000, mean = 10^8, sd = 1)

Y_i <- c()

for ( i in 1:length(x)){
  Y_i[i] <- myvar(x[1:i]) - var(x[1:i])
}

# Y_i <- Y_i[-1] # The first observation is NaN.

df1 <- data.frame( "i" = 1:length(Y_i), "Yi" = Y_i) 

library(ggplot2)

plot1 <- ggplot(df1, aes( x = i, y = Yi)) + geom_point()

plot1

# plot(1:length(x), Y_i)

myvar2 <- function(x){
  s <- sum((x-mean(x))^2)
  n <- length(x)
  variance <- s/(n-1)
  return(variance)
}

Y2_i <- c()

for ( i in 1:length(x)){
  Y2_i[i] <- myvar2(x[1:i]) - var(x[1:i])
}

Y2_i <- Y2_i[-1] # The first observation is NaN.

df2 <- data.frame( "i" = 1:length(Y2_i), "Yi" = Y2_i) 

plot2 <- ggplot(df2, aes( x = i, y = Yi)) + geom_point()

plot2

# Question 4
n <- 30000
k <- 700

A <- prod(1:n) / (prod(1:k) * prod(1:(n-k))) # large n and k give NaN

B <- prod((k+1):n) / prod(1:(n-k)) # large n and k give NaN

C <- prod(((k+1):n) / (1:(n-k))) # large n and k give Inf

