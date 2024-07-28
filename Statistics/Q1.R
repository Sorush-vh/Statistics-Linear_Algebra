library(ggplot2)
answer_vector <- vector("numeric")
temp <-0
for (i in 1:100000) {
  if(rbinom(1,1,0.2)==1){
    temp<- i/100000
    answer_vector <- append(answer_vector,temp)
  }
}
differenciation_vect<-vector("numeric")
i <-0
dif <-0
lenghthh<-length(answer_vector)-1
for (i in 1:lenghthh) {
  dif<-answer_vector[i+1]-answer_vector[i]
  differenciation_vect<-append(differenciation_vect,dif)
}
hist(differenciation_vect)


ggplot() + geom_histogram(aes(differenciation_vect), bins =45,boundary=0)

#bakhshe akhar mund
