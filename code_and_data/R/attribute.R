attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,type="af",
  dir="back",tot=TRUE,range=NULL,sim=FALSE,nsim=5000) {
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- attr(basis,"argvar")$cen
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    xlag <- if(dir=="back") dlnm:::Lag2(x,seq(lag[1],lag[2])) else 
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("")
    if(ncol(x)!=diff(lag)+1) stop("")
  }
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("")
    if(ncol(cases)!=diff(lag)+1) stop("")
    cases <- rowMeans(cases)
  } else {
    if(dir=="forw") cases <- rowMeans(dlnm:::Lag2(cases,-seq(lag[1],lag[2])))
  }
  if(!is.null(model)) {
    name <- deparse(substitute(basis))
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class,cond)
    vcov <- dlnm:::getvcov(model,model.class,cond)
  }
  red <- length(coef)!=ncol(basis)
  basisnew <- if(!red) 
    do.call(crossbasis,list(x=xlag,lag=lag,argvar=attr(basis,"argvar"),
      arglag=attr(basis,"arglag"))) else 
    do.call(onebasis,c(list(x=x),attr(basis,"argvar")))
  if(length(coef)!=ncol(basisnew))
    stop("")
  if(any(dim(vcov)!=c(length(coef),length(coef)))) 
    stop("")
  if(red&&dir=="back") stop("")
  af <- 1-exp(-rowSums(as.matrix(basisnew%*%coef)))
  an <- af*cases
  if(tot) {
    isna <- is.na(an)
    an <- sum(an,na.rm=T)
    af <- an/sum(cases[!isna])
  }
  if(!tot && sim) {
    sim <- FALSE
  }
  if(sim) {
    k <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),k) %*% t(X)
    ansim <- apply(coefsim,2, function(coefi) 
      sum((1-exp(-drop(basisnew%*%coefi)))*cases,na.rm=T))
    afsim <- ansim/sum(cases[!isna])
  }
  res <- if(sim) {
    if(type=="an") ansim else afsim
  } else {
    if(type=="an") an else af    
  }
  return(res)
}