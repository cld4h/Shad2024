# Example 2

x <- seq(-2, 2, length = 1000)
y <- x^5 - 5*x + 3
plot(x, y, type = "l", ylab = expression(x^5-5*x+3))
abline(h = 0, lty =2)

# First root

x <- -2
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x^5 - 5*x + 3)/(5 * x^4 - 5)
  print(x.new)
  x <- x.new
}
x1 <- x.new
x1

# Second root

x <- 0
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x^5 - 5*x + 3)/(5 * x^4 - 5)
  print(x.new)
  x <- x.new
}
x2 <- x.new
x2

# Third root

x <- 2
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x^5 - 5*x + 3)/(5 * x^4 - 5)
  print(x.new)
  x <- x.new
}
x3 <- x.new
x3

x <- seq(-2, 2, length = 1000)
y <- x^5 - 5*x + 3
plot(x, y, type = "l", ylab = expression(x^5-5*x+3))
abline(h = 0, lty =2)
points(x1, 0, col = "red", pch = 16, lwd =3)
points(x2, 0, col = "red", pch = 16, lwd =3)
points(x3, 0, col = "red", pch = 16, lwd =3)

# Exercise

x <- seq(0, 10, length = 1000)
y <- x^2 - 10
plot(x, y, type = "l", ylab = expression(sqrt(x)))
points(sqrt(10), 0, col = "red", pch = 16, lwd =3)

x <- 3
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x^2 - 10)/(2 * x)
  print(x.new)
  x <- x.new
}


x <- 2
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x^2 - 10)/(2 * x)
  print(x.new)
  x <- x.new
}

x <- 4
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x^2 - 10)/(2 * x)
  print(x.new)
  x <- x.new
}


# Remark

x <- seq(-4, 5, length = 1000)
y <- x/(x^2 + 1)
plot(x, y, type = "l")

x <- 2
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x * (x^2 + 1)/(1 - x^2))
  print(x.new)
  x <- x.new
}


x <- 1/sqrt(3)
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x * (x^2 + 1)/(1 - x^2))
  print(x.new)
  x <- x.new
}


x <- 1/2
i <- 1
while(i <= 10){
  i <- i + 1
  x.new <- x - (x * (x^2 + 1)/(1 - x^2))
  print(x.new)
  x <- x.new
}