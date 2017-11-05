#hw1.R by Xiaodong Gu (xgu60)

library("ggplot2")

#2. Log Gamma (loop)
log_gamma_loop <- function(n){
  if(n <= 2) return (0)
  res = 0
  for(i in 1:(n-1)){
    res = res + log(i)
  }
  return (res)
}

#3. Log Gamma (Recursive)
log_gamma_recursive <- function(n){
  if(n <= 2){
    return (0)
  }else{
    return (log(n-1) + log_gamma_recursive(n-1))
  }
}

#4. Sum of Log Gamma
sum_log_gamma_loop <- function(n){
  sum = 0
  for(i in (1:n)){
    sum = sum + log_gamma_loop(i)
  }
  return (sum)
}


sum_log_gamma_recursive <- function(n){
  if(n < 2) return (0)
  return (log_gamma_recursive(n) + sum_log_gamma_recursive(n-1))
}

#5. Compare Results to Build-In R Function
sum_lgamma <- function(n){
  sum = 0
  for(i in (1:n)){
    sum = sum + lgamma(i)
  }
  return (sum)
}

running.time1 <- function(num.seq){
  nums = c()
  loop.time = c()
  
  for(n in num.seq){
    nums = append(nums, n)
    loop.time = append(loop.time, system.time(a <- sum_log_gamma_loop(n))[[1]])
    }
  time.dataframe = data.frame(numbers = nums, time = loop.time)
  
  return (time.dataframe)
}

running.time2 <- function(num.seq){
  nums = c()
  recursive.time = c()
  for(n in num.seq){
    nums = append(nums, n)
    recursive.time = append(recursive.time, system.time(b <- sum_log_gamma_recursive(n))[[1]])
  }
  time.dataframe = data.frame(numbers = nums, time = recursive.time)
    
  return (time.dataframe)
}
    
running.time3 <- function(num.seq){
    nums = c()
    build.in.time = c()
    for(n in num.seq){
        nums = append(nums, n)
        build.in.time = append(build.in.time, system.time(c <- sum_lgamma(n))[[1]])
          
      }
        
    time.dataframe = data.frame(numbers = nums, time = build.in.time) 
    return (time.dataframe)
}
      


#print (log_gamma_loop(5))
#print (log_gamma_recursive(5))
#print (lgamma(5))
#print (sum_log_gamma_loop(50))
#print (sum_log_gamma_recursive(50))
#print (sum_lgamma(50))

options(expressions=500000)

rt1 = running.time1(c(200, 400, 800, 1600, 3200))
rt2 = running.time2(c(100, 200, 400, 800, 1600))
rt3 = running.time3(c(51200, 102400, 204800, 409600, 819200, 1638400, 3276800, 6553600, 13107200))


p <- ggplot(data = rt1, aes(numbers, time, color = "loop")) +  
  geom_point() +  
  geom_line() + 
  geom_point(data = rt2, aes(color = "recursive") ) +
  geom_line(data = rt2, aes(color = "recursive")) + 
  geom_point(data = rt3, aes(color = "build.in")) +
  geom_line(data = rt3, aes(color = "build.in")) +
  scale_x_log10() +
  scale_y_log10() +
  theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1))
  
print (p)

