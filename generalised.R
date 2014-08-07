#
# generlised
# 

generalised <- function(formula, data, family = "gaussian"){
  mc <- match.call(expand.dots = FALSE)
  cl <- model.frame(mc)
  Y <- model.response(cl)
  x <- model.matrix(cl)
  
  if(family == "gaussian"){
    b.hat <- solve(t(x)%*%x)%*%t(x)%*%Y
    return(list(mc,b.hat))
  }
  
  if(family == "binomial"){
    nn=apply(as.matrix(Y), 1, sum)
    p=dim(x)[2]
    b.hat=rep(0,p)
    k=0
    while(k < 100)
    {
      k=k+1
      pi.t=plogis(as.numeric(x%*%b.hat))
      W=diag(nn*pi.t*(1-pi.t))
      H=solve(t(x)%*%W%*%x)
      b.new=b.hat+solve(t(x)%*%W%*%x)%*%t(x)%*%(Y-nn*pi.t)
      b.old=b.hat
      b.hat=b.new
      cat(k,b.hat,"\n")
      if(sum(abs(b.old - b.hat)) < 1e-10)
      {
        cat("Model Converged \n")
        cat("Number of Newton-Raphson iterations:", k)
        break;
      } 
    } 
    return(list(k,mc,b.hat))
    } else return("family not yet implemented")   
}

generalised(groupn ~ weight, data = dat, family = "binomial")

dat$groupn <- ifelse(dat$group == "Ctl", 1, 0)  
  
model.frame(groupn ~ weight, data = dat)

lm(weight~group)


Y <- as.numeric(group)
x <- cbind(1,weight)
