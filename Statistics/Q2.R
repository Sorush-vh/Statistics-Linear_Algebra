library(ggplot2)

bufferrate<-function(n){
  total_goods<-0
  for (i in 1:10000) {
    total_goods<-total_goods+good_cond_bern(n)
  }
  probability<-total_goods/10000
  return (probability)
}

good_cond_bern<-function(n){
  dl_left<-0
  while (n>0) {
    dl_left<-dl_left+runif(1,0.6,1.4)-1
    if(dl_left<0){
      return(0)
    }
    n<-n-1
  }
  return(1)
}

probability_vector<-vector("numeric")
new_p<-0
for (i in 2:200) {
  new_p<-good_cond_est(i)
  probability_vector<-append(probability_vector,new_p)
}
df <- data.frame(p=probability_vector,length=2:200)
ggplot(df,aes(x=length,y=p))+geom_point()



#par jim:
good_cond_est_with_hs<-function(hs){
  total_goods<-0
  for (i in 1:10000) {
    total_goods<-total_goods+good_bern_with_hs(100,hs)
  }
  probability<-total_goods/10000
  return (probability)
}


good_bern_with_hs<-function(n,hs){
  dl_left<-0
  #applying headstart affect
  while (hs>0) {
    dl_left<-dl_left+runif(1,0.2,1.5)
    hs<-hs-1
  }
  #same procedure for the rest but with new uniform distribution
  while (n>0) {
    dl_left<-dl_left+runif(1,0.2,1.5)-1
    if(dl_left<0){
      return(0)
    }
    n<-n-1
  }
  return(1)
}

probability_vector2 <- vector("numeric")
new_p<-0
for (i in 0:40) {
  new_p<-good_cond_est_with_hs(i)
  probability_vector2<-append(probability_vector2,new_p)
}
df <- data.frame(p=probability_vector2,hs_val=0:40)
ggplot(df,aes(x=hs_val,y=p))+geom_point()
#after a threshold headstart (about 23s) there is a confidence level of
# >90% for a good quality video watch