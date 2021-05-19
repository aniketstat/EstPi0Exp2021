# Function for calculating expected p values under alternative

expect.alt=function(samplesize,delvalue)
{
integrand=function(x)
{
pchisq(x/delvalue,df=2*samplesize)*dchisq(x,df=2*samplesize)
}
I1=integrate(integrand,lower=0,upper=qchisq(0.5,df=2*samplesize))$value
I2=integrate(integrand,lower=qchisq(0.5,df=2*samplesize),upper=Inf)$value
return(2*(I2-I1))
}


# Main Function: Pi0 hat E

biswas_est=function(n,pi0.init,p.val,delta)
{
m=length(p.val)
m0.init=floor(pi0.init*m)
d=m-m0.init

e.hat=0
for(i in 1:m)
{
e.hat[i]=expect.alt(n[i],delta[i])
}
    
e=0
for(i in 1:d)
{
e[i]=sort(e.hat)[i]
}

ehat=mean(e)
pi0.new=min(1,max((mean(p.val)-ehat)/(0.5-ehat),0))

return(pi0.new)
}

