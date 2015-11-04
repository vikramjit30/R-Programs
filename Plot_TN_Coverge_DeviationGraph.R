library(igraph)
setwd("//terra/Public/Esch/SASO2014/Datasource/output/Aggregate_NW")

# Code to read the .txt file and put the entire data into the matrix format
# for the temporal network data
a1 <- 0
all_files <- list.files(pattern =NULL)
for (m in 1:length(all_files))
{
  
      data = read.table(all_files[m], sep='\t', header=FALSE)
      print("New turn")

      # Get the total number of nodes from the matrix by reading the first entire row
      nodes <- data[1,]
      total_nodes <- (length(nodes)-1)/2

      #print(total_nodes)

      # Calculate the total time period of the matrix

      time <- data[,1]
      total_time <- length(time) - 2

      #print(total_time)
      # calculate the total sum of all the local values from the first row (we can choose any row actually)

      sum_of_rand_values <- 0
      i <- 3
      while (i <= length(nodes))
          {
            sum_of_rand_values <- sum_of_rand_values + data[1,i]
            i = i+2
          }

      #print(sum_of_rand_values)
      #calculating the mean value of the data (which will remain constant)

      mean_value <- (sum_of_rand_values / total_nodes)

      #print(mean_value)
         sum_of_mean_deviation <- 0
  
      

      #Calculate the absolute difference of each node with the mean value and store the sum with respect to a particular time period
      
       j <- 3
         each_time_mean_deviaton <- numeric()
          for(i in 1:length(time))
              {
                   while(j<= length(nodes))
                       {
                          local_mean_difference <-  abs(mean_value - data[i,j])
                          sum_of_mean_deviation <- sum_of_mean_deviation + local_mean_difference
                          j = j+2
                       }
                 
      #Store this value to a vector in order to the plot the graph later
               if(m > 1)
                {
                  sum_of_mean_deviation <- sum_of_mean_deviation/100
                }

                  each_time_mean_deviaton <- c(each_time_mean_deviaton,sum_of_mean_deviation)
                  sum_of_mean_deviation <- 0
                  j <- 3

              } 


          ## Get the maximum range value for the Y-axis.
        
              r1 <- max(each_time_mean_deviaton)
              a1 <- c(a1,r1)
             
            
        
  }

print(a1)

a1_max <- max(a1)

print(a1_max)





######################################################################################################
# Plot the graph between the time_period and each_time_mean_deviaton, also with the aggregated network
######################################################################################################




## Change the below code appropriately

# store the time period in time_period vector

time_period <- seq(0,length(time)-2)
g_range <- range(0,a1_max)

###############################################################################################################

for (m in 1:length(all_files))
{
  
  data = read.table(all_files[m], sep='\t', header=FALSE)
  print("New turn")
  
  # Get the total number of nodes from the matrix by reading the first entire row
  nodes <- data[1,]
  total_nodes <- (length(nodes)-1)/2
  
  #print(total_nodes)
  
  # Calculate the total time period of the matrix
  
  time <- data[,1]
  total_time <- length(time) - 2
  
  #print(total_time)
  # calculate the total sum of all the local values from the first row (we can choose any row actually)
  
  sum_of_rand_values <- 0
  i <- 3
  while (i <= length(nodes))
  {
    sum_of_rand_values <- sum_of_rand_values + data[1,i]
    i = i+2
  }
  
  #print(sum_of_rand_values)
  #calculating the mean value of the data (which will remain constant)
  
  mean_value <- (sum_of_rand_values / total_nodes)
  
  #print(mean_value)
  sum_of_mean_deviation <- 0
  
  
  #Calculate the absolute difference of each node with the mean value and store the sum with respect to a particular time period
  
  j <- 3
  each_time_mean_deviaton <- numeric()
  for(i in 1:length(time))
  {
    while(j<= length(nodes))
    {
      local_mean_difference <-  abs(mean_value - data[i,j])
      sum_of_mean_deviation <- sum_of_mean_deviation + local_mean_difference
      j = j+2
    }
    
    #Store this value to a vector in order to the plot the graph later
    if(m > 1)
    {
      sum_of_mean_deviation <- sum_of_mean_deviation/100
    }
    
    each_time_mean_deviaton <- c(each_time_mean_deviaton,sum_of_mean_deviation)
    sum_of_mean_deviation <- 0
    j <- 3
    
  } 
  
  
  ## store the value of each time mean deviation into a different value of array
  ## incomplete from here..
  if (m == 1 )
  {
   
    
   plot(each_time_mean_deviaton, type="o", col="blue",ann=FALSE,ylim=g_range)
    
  }
  
  else 
    
  {  
      
      lines(each_time_mean_deviaton, type="o", pch=22, lty=2, col="red")
    
  }
  
  
}




#################################################################################################################
## Here we need to know the total number of files (this is a manual process)


#plot(each_file_mean_values[1], type="o", col="blue",ann=FALSE,ylim=g_range)


#lines(each_file_mean_values[2], type="o", pch=22, lty=2, col="red")



title(main="Collective Gossips Simulation", col.main="green", font.main=3)

title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Aggregated deviation from mean value", col.lab=rgb(0,0.5,0))


legend(1000, g_range[6], c("[1-100]","[1-1000]","[1-1000]","[1-10000]","[1-100000]","[1-1000000]"), cex=1.0, 
       col=c("blue","red","red","red","red","red"), pch=21:22, lty=1:2);
