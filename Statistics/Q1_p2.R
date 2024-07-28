library(ggplot2)
flags_loc <- runif(50000,0,1)
flags_loc<-sort(flags_loc)

differenciation_vect<-vector("numeric")
i <-0
dif <-0
lenghthh<-length(flags_loc)-1
for (i in 1:lenghthh) {
  dif<-flags_loc[i+1]-flags_loc[i]
  differenciation_vect<-append(differenciation_vect,dif)
}
hist(differenciation_vect,breaks = 15)


ggplot() + geom_histogram(aes(differenciation_vect), bins = 50,boundary=0)

