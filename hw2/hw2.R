#hw2.R by Xiaodong Gu (xgu60)

library(ggplot2)
data(midwest)
data(diamonds)

#1.Professional Employment by States

Q1 = function(){
  #generate subset has three columns: state, poptotal and percprof
  subset = midwest[, c(3, 17, 20)]

  #add new column to subset: popprof 
  subset$popprof = subset$popadults * subset$percprof / 100

  #create two vectors to store state names and their percentage of total professional employment.
  state.names = unique(subset$state)
  percprof.states = c()

  #use for loop to calculate each state, and store them to vector
  for(name in state.names){
    pop.state = sum(subset$popadults[subset$state == name])
    popprof.state = sum(subset$popprof[subset$state == name])
    percprof.state = round(popprof.state / pop.state * 100, digits=2)
    percprof.states = c(percprof.states, percprof.state)
  }

  #create dataframe for ggplot
  df = data.frame(state = state.names, percprof = percprof.states)

  #ggplot
  p = ggplot(df, aes(state, percprof, label = percprof)) +
             geom_bar(stat = "identity", fill = "grey50") +
             geom_text(colour="black", size = 6, hjust=-0.5) +
             ylim(0, 8) +
             ylab("Percentages of Professional Employment") +
             coord_flip() +
             ggtitle("Problem 1: Professional Employment by State") +
             theme(plot.title = element_text(colour="black",size=32),
                   axis.text.x = element_text(colour="black",size=16),
                   axis.text.y = element_text(colour="black",size=20),  
                   axis.title.x = element_text(colour="black",size=20),
                   axis.title.y = element_text(colour="black",size=20))
         
  print (p)
  }



#2. School and College Education by State
Q2 = function(){
  subset = midwest[, c(3, 17, 18, 19)]
  subset$pophsd = subset$popadults * subset$perchsd / 100
  subset$popcollege = subset$popadults * subset$percollege / 100
  
  state.names = unique(subset$state)
  perchsd.states = c()
  percollege.states = c()
  corr.states = c()
  
  for(name in state.names){
    pop.state = sum(subset$popadults[subset$state == name])
    pophsd.state = sum(subset$pophsd[subset$state == name])
    popcollege.state = sum(subset$popcollege[subset$state == name])
    perchsd.state = round(pophsd.state / pop.state * 100, digits=2)
    percollege.state = round(popcollege.state / pop.state * 100, digits=2)
    corr.state = round(cor(subset$perchsd[subset$state==name], subset$percollege[subset$state==name]), digits=2)
    perchsd.states = c(perchsd.states, perchsd.state)
    percollege.states = c(percollege.states, percollege.state)
    corr.states = c(corr.states, corr.state)
  }
  
  df = data.frame(state = state.names, perchsd = perchsd.states, percollege = percollege.states, corr = corr.states)
  print(head(df, 5))
  corr_coef = paste("italic(r) == ", round(cor(df$perchsd, df$percollege), digits=2))
  
  #bar plot of percentages of different education in states 
  p = ggplot(df, aes(state, perchsd, label = perchsd, fill = "High School")) +
    geom_bar(stat="identity", position = "dodge") +
    geom_text(colour="black", size = 6, vjust=3 ) +
    geom_bar(aes(state, percollege, fill = "College"), stat="identity", position = "dodge") +
    geom_text(aes(label = percollege), colour="black", size = 6, vjust=20) +
    ylim(0, 100) + 
    ggtitle("Problem 2: School and College Education by State") +
    ylab("Percentage of Education") +
    labs(fill = "") +
    theme(plot.title = element_text(colour="black",size=32),
          axis.text.x = element_text(colour="black",size=16),
          axis.text.y = element_text(colour="black",size=20),  
          axis.title.x = element_text(colour="black",size=20),
          axis.title.y = element_text(colour="black",size=20))
    
    
  #scatter plot of correlation of different education in states
  p2 = ggplot(df, aes(perchsd, percollege)) +
    geom_point(size=5) +
    geom_smooth(colour = "red", fill = "lightgreen", method = 'lm') +
    geom_text(x = 78, y = 26, label = corr_coef, parse = TRUE, size = 10) +
    ggtitle("Problem 2: Correlation between Educations") +
    xlab("Percentage of High School diploma") +
    ylab("Percentage of College Education") + 
    theme(plot.title = element_text(colour="black",size=32),
          axis.text.x = element_text(colour="black",size=20),
          axis.text.y = element_text(colour="black",size=20),  
          axis.title.x = element_text(colour="black",size=20),
          axis.title.y = element_text(colour="black",size=20))
  
  #scatter plots of different education in different counties of different states
  p3 = ggplot(subset, aes(perchsd, percollege)) +
    geom_point(size=5) +
    geom_smooth(colour = "red", fill = "lightgreen", method = 'lm') +
    facet_grid(.~subset$state) +
    ggtitle("Problem 2: Correlation between Educations in Each State") +
    xlab("Percentage of High School diploma") +
    ylab("Percentage of College Education") + 
    theme(plot.title = element_text(colour="black",size=28),
          axis.text.x = element_text(colour="black",size=12),
          axis.text.y = element_text(colour="black",size=12),  
          axis.title.x = element_text(colour="black",size=20),
          axis.title.y = element_text(colour="black",size=20)) 
    
  print (p)
  print (p2)
  print (p3)
}

#3. Comparison of visulization techniques
Q3 = function(){
  #create a subset of OH state
  df.OH = midwest[midwest$state == "OH",]
  
  #Boxplot of percprof of OH
  p1 = ggplot(df.OH, aes(state, percprof)) +
    geom_boxplot() +
    coord_flip()
  
  #histogram of percprof of OH
  p2 = ggplot(df.OH, aes(percprof)) +
    geom_histogram(bins=50)  +
    facet_wrap(~state)
  
  #QQplot of percprof of OH
  p3 = ggplot(df.OH, aes(sample=percprof) ) +
    stat_qq()
    
  #boxplot of percprof of midwest states
  p4 = ggplot(midwest, aes(state, percprof)) +
    geom_boxplot()
    
  #histogram of of percprof of midwest states
  p5 = ggplot(midwest, aes(percprof)) +
    geom_histogram(bins=50)  +
    facet_wrap(~state)
  
  #qqplot of of percprof of midwest states
  p6 = ggplot(midwest, aes(sample=percprof) ) +
    stat_qq() +
    facet_wrap(~state)
  
  print (p1)
  print (p2)
  print (p3)
  print (p4)
  print (p5)
  print (p6)
}

#4. Random Scatterplots
Q4 = function(){
  #location to save files
  dir = "C:/Users/sheldon/Desktop/"
  
  points = c(1000, 2000, 4000, 8000,16000,32000,64000, 128000)
  
  #create vectors to store file size
  ps.size = c()
  pdf.size = c()
  jpeg.size = c()
  png.size = c()
  
  for(point in points){
    df = df.runif(point)
    p = ggplot(df, aes(X, Y)) + geom_point()
  
    ggsave(paste(dir, "p4.ps"))
    ggsave(paste(dir, "p4.pdf"))
    ggsave(paste(dir, "p4.jpeg"))
    ggsave(paste(dir, "p4.png"))
    ps.size = c(ps.size, file.info(paste(dir, "p4.ps"))$size)
    pdf.size = c(pdf.size, file.info(paste(dir, "p4.pdf"))$size)
    jpeg.size = c(jpeg.size, file.info(paste(dir, "p4.jpeg"))$size)
    png.size = c(png.size, file.info(paste(dir, "p4.png"))$size)
  }
  
  df = data.frame(N = points, ps = ps.size, pdf = pdf.size, jpeg = jpeg.size, png = png.size)
  
  p = ggplot(df, aes(N, ps,  color = "ps")) +
    geom_point(size=5) +
    geom_line(size=1) +
    geom_point(aes(N, pdf, color = "pdf"), size=5) +
    geom_line(aes(N, pdf, color = "pdf"), size=1) +
    geom_point(aes(N, jpeg, color = "jpeg"), size=5) +
    geom_line(aes(N, jpeg, color = "jpeg"), size=1) +
    geom_point(aes(N, png, color = 'png'), size=5) +
    geom_line(aes(N, png, color = 'png'), size=1) +
    #scale_x_log10() +
    #scale_y_log10() +
    ggtitle("Problem 4: plot file size vs N") +
    xlab("N") +
    ylab("plot file size") + 
    labs(color="", size=20) +
    theme(plot.title = element_text(colour="black",size=28),
          axis.text.x = element_text(colour="black",size=16),
          axis.text.y = element_text(colour="black",size=16),  
          axis.title.x = element_text(colour="black",size=20),
          axis.title.y = element_text(colour="black",size=20),
          legend.key.size = unit(1.5, "cm"),
          legend.text = element_text(size = 20),
          legend.position = "top") 
  print (p)
  
}

#generate a new dataframe with column X and column Y 
df.runif = function(N){
  x = runif(N, 0, 20)
  y = runif(N, -1, 50)
  df = data.frame(X = x, Y = y)
  return (df)
  
}

#5. Diamonds
Q5 = function(){
  #create new column carat2 to round carat of each diamonds
  diamonds$carat2 = round(diamonds$carat, digits=1)
  
  #histogram for color
  p1 = ggplot(diamonds, aes(color)) +
    geom_histogram(stat="count")
  
  #histogram for carat
  p2 = ggplot(diamonds, aes(carat)) +
    geom_histogram(bins=50)
  
  #histogram for price
  p3 = ggplot(diamonds, aes(price)) +
    geom_histogram(bins=50)
  
  #distribution of price in each color
  p4 = ggplot(diamonds, aes(color, price)) +
    geom_boxplot() +
    facet_wrap(~carat2) 
    
  #correlation between carat and price
  p5 = ggplot(diamonds, aes(carat, price)) +
    geom_point() +
    facet_grid(.~color) +
    geom_smooth(colour = "red", fill = "lightgreen", method = 'lm')
  
  #distribution of carat in each color
  p6 = ggplot(diamonds, aes(color, carat)) +
    geom_boxplot() 
    
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  print(p6)
  
}

#please uncomment the following lines 
#Q1()
#Q2()
#Q3()
#please change dir in Q4() before run the function
#Q4()
#Q5()






