#' @export
derivs <- function(m=model,x=x,eps=0.01) {
  pre 	<- predict(m,type='response',se.fit=T)
  mod1 	<- predict(m,type='lpmatrix')
  newx 	<- data.frame(x=x-eps)
  mod2 	<- predict(m,newx,type='lpmatrix')
  modp 	<- (mod1-mod2)/eps
  fd_mod 	<- modp %*% coef(m) #first derivative
  model2 	<- gam(fd_mod~s(x,k=20))
  mod3 	<- predict(model2,type='lpmatrix')
  mod4 	<- predict(model2,newx,type='lpmatrix')
  modpp 	<- (mod3-mod4)/eps
  sdmod 	<- modpp %*% coef(model2) 
  return(list(fd=fd_mod,sd=sdmod))
}
