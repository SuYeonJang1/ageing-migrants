
pdiff <- function(object, at, by, level = 0.95){
  library(prediction)
  source("function/within_intercept2.R")
  data <- find_data(object, parent.frame())
  model <- object
  args <- model$args
  if(!inherits(model, "plm")){
    stop("input 'object' needs to be a model estimated by plm()")
  }else if(is.null(at)){
    stop("at specification is required")
  }else{
    if(args$model == "within"){
      fe <- within_intercept2(object, return.model = TRUE)
      model$coefficients <- fe$coefficients
      model$vcov <- fe$vcov
      names(model$coefficients) <- c("(Intercept)", names(object$coefficients))
      colnames(model$vcov) <- c("(Intercept)", colnames(object$vcov))
      rownames(model$vcov) <- c("(Intercept)", rownames(object$vcov))
    }
    datalist <- build_datalist(data, at = at, as.data.frame = TRUE)
    at_spec <- attr(datalist, "at_specification")
    at_spec <- data.frame(at_spec[,-which(names(at_spec)=="index")])
    colnames(at_spec) <- names(at)
    Terms <- delete.response(terms(model))
    m <- model.frame(Terms, datalist)
    m <- split(m,rep(1:nrow(at_spec),rep(nrow(data),nrow(at_spec))))
    X <- lapply(m, function(mm)model.matrix(Terms, mm))
    at_by <- at_spec[,names(by)]
    gg <- lapply(unique(at_by), function(x){
      which_by <- which(at_by==x)
      g <- X[which_by]
      grad_g <- Reduce("rbind",lapply(g, function(m)apply(m,2,mean)))
    })
    atbywhich <- which(unique(at_by)==by)
    grad_g <- lapply(gg[1:length(gg)], function(x)x-gg[[atbywhich]])
    grad_g <- Reduce("rbind",grad_g)
    coef <- coef(model)[which(names(coef(model)) %in% colnames(grad_g))]
    vcov <- vcov(model)[which(names(coef(model)) %in% colnames(grad_g)),
                        which(names(coef(model)) %in% colnames(grad_g))]
    grad_g <- grad_g[,which(colnames(grad_g) %in% names(coef))]
    fitted <- grad_g %*% coef
    fitted <- cbind(at_spec, fitted)
    colnames(fitted) <- c(names(at), "diff")
    se <- apply(grad_g, 1, function(j){
      sqrt(t(j) %*% vcov %*% t(t(j)))
    })
    if(args$model %in% c("within","random")){
      score <- abs(qnorm((1-level)/2))
    }else{
      score <- abs(qt(p=(1 - level)/2, df=nrow(data)-1))
    }
    margin.error <- as.numeric(score * se)
    lwr <- fitted[,"diff"] - margin.error
    upr <- fitted[,"diff"] + margin.error
    result <- cbind(fitted, se, lwr, upr)
    colnames(result) <- c(colnames(fitted), "se", "lwr", "upr")
    row.names(result) <- NULL
    result <- data.frame(result)
    return(result)
  }
}
