library(ggplot2)
log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

constant.time <- function(){
  count = 0
  for(i in seq(1:50)){
    for(j in seq(1:50)){
      count = count + 1
    }
  }
  return (0)
}

running.time <- function(function.name, num.seq){
  nums = c()
  time = c()
  for(n in num.seq){
    nums = append(nums, n)
    time = append(time, system.time(a <- function.name(n))[[1]])
  }
  time.dataframe = data.frame(numbers = nums, time = time)
  return (time.dataframe)
}

options(expressions=500000)
#rt = running.time(log_factorial, c( 200, 400, 800,1200,1600, 2000))
#rt = running.time(sum_log_factorial, c( 200, 400, 600, 800,1200, 1400, 1600, 1800))
rt = running.time(fibonacci, c(20, 22, 24, 26, 28, 30))

p <- ggplot(data = rt, aes(numbers, time)) +  
  geom_point() +  
  geom_line() +
  scale_y_log10()

print (p)

