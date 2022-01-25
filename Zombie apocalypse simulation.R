library(tidyverse)

#===============================================================================
#           Creating the population
#===============================================================================
pop_val <- 1000

x <- rnorm(pop_val, mean = 60, sd = 20)
x <- round(x, digits = 1) #x cordinate values


y <- rnorm(pop_val, mean = 80, sd = 10)
y <- round(y, digits = 1) #y cordinate values

state <- rep("normal", pop_val) #Status, initially set to normal

imm_prob_val <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
immunity <- sample(imm_prob_val, pop_val, replace = TRUE)

data <- data.frame(x,y, state, immunity) # joined as a data frame
plot(data$x, data$y, col = 'green')

#==============================================================================
#               People movement generation 
#==============================================================================

#This section, it just moves the people around the plane and produces multiple 
#Plots

i = 1
loop = 1

for (loop in 1:30) {
  for (i in 1:pop_val){ 
    temp_x <- round(runif(1, -1.0, 1.0 ), digits = 1)
    temp_y <- round(runif(1, -1.0, 1.0), digits = 1)
    
    data$x[i] <- data$x[i] + temp_x
    data$y[i] <- data$y[i] + temp_y
    i = i + 1
    
  } 
  plot(data$x, data$y, col = 'green')
  loop = loop + 1
  
}

#==============================================================================
#Creating a random person who got infected
#==============================================================================

data$state[sample(1:pop_val, 1)] <- "Infected"

plot(data$x, data$y, xlab = "X-Axis", ylab = "Y-Axis", col = ifelse(data$state == 'normal', 'green', 'red'))

#===============================================================================
#The infection spread
#===============================================================================

zombie <- data.frame(subset(data, data$state == "Infected"))

tmp = 0
j = 1

for (j in 1:200) {
  
  tmp = round(runif(1, -5.0, 5.0 ), digits = 1)
  print(paste("Main Loop Number ", j, "   Random value Generated : ", tmp))
  print("==================================================")
    
  zombie$x <- zombie$x + tmp 
  zombie$y <- zombie$y + tmp
  
  print(paste("Number ", j, "X value : ", zombie$x))
  print(paste("Number ", j, "y value : ", zombie$y))
  print("==================================================")
  
  k = 1
  
  print(paste("Inside the data Loop "))
  print("====================================================================")
  print("====================================================================")
  
  for (k in 1:nrow(data)) {
    
    if (data$state[k] == "Infected")
      print("This person is already infected, back away")
    else {
      d_x = 0
      d_y = 0 
      print(paste("Number ", k , "  Main Loop : ", j))
      print("========================================")
      print(paste("x value : ", data$x[k]))
      print(paste("x value : ", data$y[k]))
      print("========================================")
      
      d_x = round((data$x[k] - zombie$x), digits = 1)
      d_y = round((data$y[k] - zombie$y), digits = 1)
      
      print(paste("Difference in x value : ", d_x))
      print(paste("Difference in y value : ", d_y))
      
      if(d_x <= 3 & d_x >= -3){
        print(paste(k," 's X value is satisfied here"))
        if(d_y <= 3 & d_y >= -3){
          print(paste(k, " 's y value is satisfied here as well "))
          if(data$immunity[k] <= 0.5){
            print("This person got the virus")
            data$state[k] <- "Infected"
          }
          else
            print("Person has good immunity, so no effect")
        }
        else
          print(paste(k," 's y value is not satisfied here as well"))
      }
      
      else
        print(paste(k, " 's X value itself is not satisfied here"))
      
      print(paste("Value ", k," check done "))
      print ("===========================================================")
      
    }
    k = k + 1
  }
  
  plot(data$x, data$y, xlab = "X-Axis", ylab = "Y-Axis", col = ifelse(data$state == 'normal', 'green', 'red'))
  j = j + 1
}

#===============================================================================
# To clear the workspace, packages, plots and consoles
#===============================================================================



# To Clear workspace
rm(list = ls())

# To Clear packages
p_unload(psych, GPArotation)

# Clear plots
dev.off()

# Clear console
cat("\014")  # ctrl+L