############################################################
#############Calculate the RPs of F-dsitribution  
F_RP <- function(m,df1,df2,tol.err = 10e-12,max_iter = 10000){
  a=1+(1:m-1)/(m-1)
  J=matrix(0,m,m)
  g = rep(0,m)
  made_changes = TRUE
  iterations=0
  while(made_changes & (iterations < max_iter)){
    iterations <- iterations +1
    made_changes <- FALSE
    ##########################J
    J[1,1] = pf((a[1]+a[2])/2,df1,df2)-df((a[1]+a[2])/2,df1,df2)*(a[2]-a[1])/4
    J[m,m] = 1 - pf((a[m-1]+a[m])/2,df1,df2)-df((a[m-1]+a[m])/2,df1,df2)*(a[m]-a[m-1])/4
    for (k in 2:(m-1)) {
      J[k,k]=pf((a[k]+a[k+1])/2,df1,df2)-pf((a[k-1]+a[k])/2,df1,df2)-df((a[k-1]+a[k])/2,df1,df2)*(a[k]-a[k-1])/4-df((a[k]+a[k+1])/2,df1,df2)*(a[k+1]-a[k])/4
    }
    
    for (k in 1:(m-1)){
      J[k+1,k] = -df((a[k]+a[k+1])/2,df1,df2)*(a[k+1]-a[k])/4
      J[k,k+1] = -df((a[k]+a[k+1])/2,df1,df2)*(a[k+1]-a[k])/4
    }
    ###################################g
    
    g[1] = a[1]*(pf((a[1]+a[2])/2,df1,df2))-Int(0,(a[1]+a[2])/2,df1,df2)
    g[m] = a[m]*(1-pf((a[m-1]+a[m])/2,df1,df2))-Int((a[m-1]+a[m])/2,Inf,df1,df2)
    
    for (k in 2:(m-1)){
      g[k] = a[k]*(pf((a[k]+a[k+1])/2,df1,df2)-pf((a[k-1]+a[k])/2,df1,df2))-Int((a[k-1]+a[k])/2,(a[k]+a[k+1])/2,df1,df2)
    }
    newa = a - solve(J)%*%g
    newa = sort(newa)
    made_changes = max(abs(g)) > tol.err
    a=newa
  }
  if (made_changes)
  {
    warning("Newton’s method terminated before convergence")
  }
  a_1 = a[1:(m-1)]
  a_2 = a[2:m]
  aa = (a_1+a_2)/2
  pp = pf(aa,df1,df2)
  p = c(pp[1],diff(pp),1-pp[m-1])
  return(list(RPs=a, Prob=p, teminals=aa,iterations=iterations,converged=!made_changes))
}


###############function to calculate the left integration x*\phi(x) from  lo to up
Int <- function(lo,up,df1,df2)
{
  f <- function(x) {
    f= x*df(x,df1,df2)
    return(f)
  }
  result = integrate(f,lo,up)
  return(result$value)
}



#this program is devised by Li Run-Ze and revised by Liang Jiajuan
#n is the sample size, p=dim
#N, r and s are parameters
#this program requires (2*N+d-2)/s must be an integer

KotzNrs <- function(n,p,N,r,s)#  an n times p matrix
{
  #This program is used to generate p-dimensional 
  # Kotz-type distribution with N (integer), s=1, r;
  # The definition of Kotz-type distribution can see Fang-Kotz-Ng (1990),
  # Section 3.2. The sample size is n.
  #if (2*N+p-2)/s==floor((2*N+p-2)/s)
  #R=sqrt(xisquare(n,1,(p+2*N-2)/s)/2/r).^(1/s)
  a=(2*N+p-2)/(2*s)
  b=1/r
  R=rgamma(n,shape = a,rate=r)^(0.5/s)
  uniform = spu(n,p)  #uniform=spu(n,p);  %uniform distr. on the unit sphere of R^d
  y = uniform*(R%*%t(rep(1,p)))#y=uniform.*(R*ones(1,p));
  return(y)
}









# a subroutine for generating NT-Net set on a unit ciricle of
# 2-dimension space
# subfile name: speven2.m

speven2u <-function(n,vec=vector())
{
#acturally, vec=[], a empty vector
a=seq(1,2*n-1,2)/n/2  #a=[1:2:2*n-1]'/n/2;  a net set on [0,1] of 1-dimension space
b=2*pi*a
x=matrix(rep(0,n*2),n,2) # zeros(n,2);
x[,1]=cos(b)
x[,2]=sin(b)
return(x)
}



#Subroutines for generating the uniform distr. on the unit sphere of
# R^p, where p is an even number
# x is nxp matrix

spevenu <-function(n,p)
{
  #C=glp(n,vec);  % length of vec is s
  C=matrix(runif(n*(p-1)),n)#C=rand(n,p-1)
  s=p-1
  p1=(s+1)/2
  power = rep(1,n)%*%t(1:(p1-1))    #power=ones(n,1)*[1:p1-1];
  power = 1/power        #power=1 ./ power;
  Cp = C[,1:(p1-1)]^power                    #Cp=C(:,1:p1-1).^power;
  if (p1==2){Hp=Cp[,1]}else
  {
    Cp = apply(t(Cp),2,rev)     #Cp=rot90(Cp);  % counterclock 90 degree of a matrix
    Cp = apply(Cp,2,cumprod)    #Cp=cumprod(Cp); % cumulative products for every columns
    Hp = t(apply(Cp,2,rev))    #Hp=rot90(Cp,3);
  }
  DH1 = cbind(rep(0,n),Hp)      #DH1=[zeros(n,1) Hp];
  DH2 = cbind(Hp,rep(1,n))      #DH2=[Hp ones(n,1)];
  D = sqrt(DH2-DH1)             #D=sqrt(DH2-DH1);   
  G = 2*pi*C[,p1-1+(1:p1)]      #G=2*pi*C(:,p1-1+[1:p1]);
  odd = 2*(1:p1)-1
  even = 2*(1:p1)
  x = matrix(0,n,2*p1)          #x=zeros(n,2*p1);
  x[,odd] = D[,1:p1]*cos(G)     #x(:,odd)=D(:,1:p1).*cos(G);
  x[,even] = D[,1:p1]*sin(G)    #x(:,even)=D(:,1:p1).*sin(G);
  return(x)
}


 
# a subroutine for generating uniform distr. on the unit sphere (ball surface)
# of 3-dimensional space, R^3

spodd3u <-function(n)       #x is nx3 matrix
{
  C = matrix(runif(n*2),n)       #C=rand(n,2);
  a = 2*sqrt(C[,1]*(1-C[,1])) #a=2*sqrt(C(:,1).*(1-C(:,1)));
  b = 2*pi*C[,2]              #b=2*pi*C(:,2);
  x = matrix(0,n,3)            #x=zeros(n,3);
  x[,1] = 1-2*C[,1]          # x(:,1)=1-2*C(:,1);
  x[,2] = a*cos(b)
  x[,3] = a*sin(b)
  return(x)
}





# Subroutines for generating the uniform distr. on the unit sphere of
# R^p, p=dim of the sphere is odd and larger than 3

spoddu <-function(n,p) # x is nxp matrix
{
  C=matrix(runif(n*(p-1)),n)#C=rand(n,p-1)
  s = p-1
  p1 = s/2
  power = rep(1,n)%*%t(seq(3,2*p1-1,2)) #power = ones(n,1)*[3:2:2*p1-1]
  power = 2/power         #power=2 ./ power;
  Cp=C[,1:(p1-1)]^power   #Cp=C(:,1:p1-1).^power;
  if (p1==2){Hp=Cp[,1]}else
  {
    Cp = apply(t(Cp),2,rev)     #Cp=rot90(Cp);  % counterclock 90 degree of a matrix
    Cp = apply(Cp,2,cumprod)    #Cp=cumprod(Cp); % cumulative products for every columns
    Hp = t(apply(Cp,2,rev))    #Hp=rot90(Cp,3);
  }
  DH1 = cbind(rep(0,n),Hp)      #DH1=[zeros(n,1) Hp];
  DH2 = cbind(Hp,rep(1,n))      #DH2=[Hp ones(n,1)];
  D = sqrt(DH2-DH1)             #D=sqrt(DH2-DH1);  
  ap=C[,p1]*(1-C[,p1])          #ap=C(:,p1).*(1-C(:,p1));
  ap1=2*pi*C[,(p1+1)]           #ap1=2*pi*C(:,p1+1);
  x=matrix(0,n,s+1)               #x=zeros(n,s+1);
  x[,1] = D[,1]*(1-2*C[,p1])       #x(:,1)=D(:,1).*(1-2*C(:,p1));
  x[,2] = 2*D[,1]*sqrt(ap)*cos(ap1) #x(:,2)=2*D(:,1).*sqrt(ap).*cos(ap1);
  x[,3] = 2*D[,1]*sqrt(ap)*sin(ap1) #x(:,3)=2*D(:,1).*sqrt(ap).*sin(ap1);
  G = 2*pi*C[,2*(2:p1)];             # n*(p1-1) matrix G=2*pi*C(:,2*[2:p1]); 
  even = 2*(2:p1)
  odd = 2*(2:p1)+1
  x[,even] = D[,2:p1]*cos(G)     #x(:,even)=D(:,2:p1).*cos(G);
  x[,odd] = D[,2:p1]*sin(G)     #x(:,odd)=D(:,2:p1).*sin(G);
  return(x)
}



# a program for generating the uniform distri. on the unit sphere of R^p
# by number-theoretic methods described in Fang and Wang's book p.166-170

spu <- function(n,p)
{
  #n: the sample size
  # p is the dimension of the sphere
  # x is nxp matrix
  if (p > 3)
  {
    if (p%%2==0){x = spevenu(n,p)} 
    else {x=spoddu(n,p)}
  }else
  {
    if (p==1) {x=2*matrix(runif(n*p),n)-1}
    else if (p==2) {x = speven2u(n)}
    else {x = spodd3u(n)}
  }
  return(x)
}



#this program is for generating beta-generalized normal distribution N(0,I_p,beta) with
#density b^p*r^(p/b)/[2^p*gamma^p(1/b)]*exp{-r*\sum_{i=1}^p|x_i|^p}, b=beta

betagnor <-function(n,s,p)
{
  z=matrix(runif(n*s),n,s)        #z=rand(n,s);
  #  y=sign(2*z-1)*(p*gaminv((2*z-1).*sign(2*z-1),1/p,1))^(1/p) #y=sign(2*z-1).*(p*gaminv((2*z-1).*sign(2*z-1),1/p,1)).^(1/p);
  
  # py=abs(y').^p;
  #npy=sum(py).^(1/p);
  #uy=y'./(ones(s,1)*npy);
  #         x=uy';
  #yr=chi2rnd(8*s,n,1);
  #R=(2*yr).^4;
  #x=diag(R)*x; %l_p norm normal
  
  
}




















