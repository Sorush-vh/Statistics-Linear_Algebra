#alef: x: time pokhte har vajh ->  30<x<90
# pas --> T(total): time kole pokhtane sahih = pkhte sahih 6 vajh
#-> 180 < T(total) < 540
library(ggplot2)

sides<- 1:6
total_time<-360

one_potato_simulation<-function(taft_count){
  
}



mat<-matrix(0,nrow = 20, ncol=6)
mat[1,2]<-mat[1,2]+5
cookTime <- matrix(0,nrow = 20, ncol=6)

sectionSimulate <- function(cookTime, n_pieces, sectionTime, sides) {
  selected_cook_side<-0
  for (i in 1:20) {
    selected_cook_side<-sample(sides,1)
    cookTime[i,selected_cook_side]<-cookTime[i,selected_cook_side]+sectionTime
  }
  return (cookTime)
}

isSideCookedWell<-function(burnTime,underCookTime,cookTime){
  if(cookTime>=underCookTime&& burnTime>=cookTime)
    return(T)
  else{
    return(F)
  }
}

cookTest <- function(underCookTime, burnTime, cookTime) {
  
  for (i in 1:20) {
    for (j in 1:6) {
      if(cookTime[i,j] < underCookTime || cookTime[i,j] > burnTime){
        return(F)
      }
    }
  }
  
  return (T)
}

cookSimulation <- function(underCookTime,burnTime,n_pieces,timeLength,sides,n_shuffle){
  freq<-timeLength/n_shuffle
  cycle_count<-0
  while (cycle_count<n_shuffle) {
    cookTime<-sectionSimulate(cookTime, n_pieces, freq, sides)
    cycle_count<-cycle_count+1
  }
  return(cookTest(underCookTime,burnTime,cookTime))
}



test_result<-function(shuffle_num){
  #run test for 10000 times
  success_count<-0
  for (i in 1:50) {
    cookTime <- matrix(0,nrow = 20, ncol=6)
    if(cookSimulation(underCookTime = 180,burnTime = 540,n_pieces = 20,
                      timeLength = 360,sides=1:6,n_shuffle = shuffle_num)){
      success_count<-success_count+1
    }
  }
  probability<-success_count/50
  return(probability)
}


shuffle_vect<-vector("numeric")
probability_vect<-vector("numeric")

for (i in 0:64) {
  shuffle_num<-80+5*i
  shuffle_vect<-append(shuffle_vect,shuffle_num)
  probability_vect<-append(probability_vect,test_result(shuffle_num))
}


df <- data.frame(p=probability_vect,shuffle=shuffle_vect)
ggplot(df,aes(x=shuffle,y=p))+geom_point()
