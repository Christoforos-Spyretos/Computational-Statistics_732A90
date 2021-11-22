fgenbeta<-function(c){
    x<-NA
    num.reject<-0
    while (is.na(x)){
	y<-runif(1)
	u<-runif(1)
	if (u<=dbeta(y,2,7)/c){x<-y}
	else{num.reject<-num.reject+1}
    }
    c(x,num.reject)
}

y<-dbeta(seq(0,2,0.001),2,7)
c<-max(y)

