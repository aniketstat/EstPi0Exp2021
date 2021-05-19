# Function for calculating number of p values greater than lambda

W=function(x,cut)
{
sum(ifelse(x>cut,1,0))
}

# Main function: Pi0 hat U

cheng_est=function(n,pi0.init,p.val,delta)
{
m=length(p.val)
m0.init=floor(pi0.init*m)
d=m-m0.init

lambda.index=seq(from=0.20,to=0.50,by=0.05) 
m0.cheng.lambda=0
Qhat=0
for(j in 1:length(lambda.index))
{
##calculating upper tail probability of 
Q.hat=0
for(i in 1:m)
{
Q.hat[i]=pchisq(qchisq(1-lambda.index[j]/2,df=2*n[i])/delta[i],df=2*n[i])-pchisq(qchisq(lambda.index[j]/2,df=2*n[i])/delta[i],df=2*n[i])
}
  
Q=0
for(i in 1:d)
{
  Q[i]=sort(Q.hat)[i]
}
  
Qhat=mean(Q)
  
 m0.cheng.lambda[j]=min(1,max(((W(p.val,lambda.index[j])-m*Qhat)/((1-lambda.index[j])-Qhat))/m,0))
}
pi0.cheng=mean(m0.cheng.lambda)

return(pi0.cheng)
}
