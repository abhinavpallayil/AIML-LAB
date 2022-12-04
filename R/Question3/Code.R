##https://rpubs.com/jrnorton11/FeedForwardBackPropagation

sink("./Output.txt",append=T)

beta <- 0.45
alpha <- 0.9
input <- N0 <- matrix(c(1,1))
t <- 1
w0 <- matrix(c(.4,-.1,.1,-.1), nrow=2)
w1 <- matrix(c(0.06, -0.4), nrow=1)

sprintf("Input is:")
print(input)
sprintf("Target is: %f",t)
sprintf("w0 is:")
print(w0)
sprintf("w1 is:")
print(w1)

sigma <- function(t) 1/(1+exp(-t))

N1 <- sigma(w0 %*% input)
sprintf("N1 is:")
print(N1)

N2 <- sigma(w1 %*% N1)

N2.0.error <- N2 * (1-N2) * (1-N2)
sprintf("N2.0.error is: %f",N2.0.error)

w1.Rate = (beta * N2.0.error[1,1]) * t(N1)
sprintf("w1.Rate is:")
print(w1.Rate)

sprintf("w1 is:")
print(w1)
w1.new <- w1 + w1.Rate + alpha*(t-1)
sprintf("w1.new is:")
print(w1.new)

N1.0.error <- N2.0.error %*% w1.new

w0.Rate = t(beta * N1.0.error) * (N0)
sprintf("w0.Rate is:")
print(w0.Rate)

w0.Rate <- matrix(c(w0.Rate[1,1], w0.Rate[2,1], w0.Rate[1,1], w0.Rate[2,1]), nrow=2)
sprintf("w0.Rate is:")
print(w0.Rate)

w0.new <- w0 + w0.Rate + alpha*(t-1)
sprintf("w0.new is:")
print(w0.new)

w0 <- w0.new
N1 <- sigma(w0 %*% input)

w1 <- w1.new
N2 <- sigma(w1 %*% N1)

sprintf("N2 is:")
print(N2)

N2.1.error <- N2 * (1-N2) * (1-N2)
sprintf("N2.1.error is: %f",N2.1.error)

sink()