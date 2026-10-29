#' @export

sdmTMB_cv_stats <- function(response_var,offset_var,denom_var) {
  
  x <- as.data.frame(matrix(data = NA,nrow = 1,ncol = length(columns)))
  colnames(x) <- columns
  
  x$Model <- mod.label
  x$Formula <- m$formula[1]
  
  x$`Time-varying` <- ifelse( is.null(m$time_varying),NA,  paste(m$time_varying[1], m$time_varying[2]))
  z  <- paste0(m$family$family,"(link = ",m$family$link,")" )
  if(length(z)>1) z = paste(z,collapse=" + ")
  x$Family = z
  x$cAIC <- cAIC(m, what = "cAIC")
  x$AIC <- AIC(m)
  x$rho <- tryCatch(m$sd_report[[1]]["rho"],error = function(e) NA)
  x$`Matern range` <- tryCatch(m$sd_report[[1]]["range"], error = function(e) NA)
  x$`Spatial SD` <- tryCatch(  m$sd_report[[1]]["sigma_O"],   error = function(e) NA)
  x$Hessian_positive <- m$pos_def_hessian
  ## CV log-likelihood
  x$`Sum loglik` <- m_cv$sum_loglik
  fitTT <- dplyr::bind_rows(sdmTMBcv_predictions( m_cv,response_var = response_var, offset_var = offset_var,fam = m$family$family,denom_var=denom_var))
  fam <- m$family$family
  train_dat <- subset(fitTT, tt == "train")
  test_dat  <- subset(fitTT, tt == "test")
  
  train_stats <- get_cv_scores(train_dat, fam)
  test_stats  <- get_cv_scores(test_dat, fam)
  
  for (nm in names(train_stats)) {
    colname <- paste0(nm, "_train")
    if (!(colname %in% names(x))) {
      x[[colname]] <- NA
    }
    x[[colname]] <- train_stats[[nm]]
  }
  
  for (nm in names(test_stats)) {
    colname <- paste0(nm, "_test")
    if (!(colname %in% names(x))) {
      x[[colname]] <- NA
    }
    
    x[[colname]] <- test_stats[[nm]]
  }
  
  x
}

sdmTMBcv_predictions <- function(object, response_var,denom_var, offset_var = NULL,fam) {
  
  x <- object$data
  k <- length(object$models)
  
  out <- vector("list", k)
  
  for(i in seq_len(k)) {
    
    mod <- object$models[[i]]
    
    train <- subset(x, fold_id != i)
    test  <- subset(x, fold_id == i)
    
    if(is.null(offset_var)) {
      pr <- predict(mod, newdata = train)
    } else {
      stopifnot(offset_var %in% names(train))
      pr <- predict(
        mod,
        newdata = train,
        offset = train[[offset_var]],
        type='response'
      )
    }
    
    train$obs  <- train[[response_var]]
    train$pred <- pr$est
    
    if(fam=='binomial'){
      ii = which(train[[denom_var]]>0)
      train = train[ii,]
      train$obs  <- train[[response_var]] / train[[denom_var]]
      train$pred <- pr$est[ii]
      
      }
    ###need to add a denominatory variable if binomial if()
    train$tt   <- "train"
    
    test$obs  <- test[[response_var]]
    test$pred <- test$cv_predicted
    
    if(fam=='binomial'){
         ii = which(test[[denom_var]]>0)
        test = test[ii,]
        test$obs  <- test[[response_var]] / test[[denom_var]]
        test$pred = test$cv_predicted
    }
    test$tt   <- "test"
    out[[i]] <- rbind(train, test)
  }
  
  dplyr::bind_rows(out)
}


cv_metrics <- function(obs, pred) {
  
  data.frame(
    n = length(obs),
    MAE = Metrics::mae(obs, pred),
    RMSE = Metrics::rmse(obs, pred),
    Bias = mean(pred - obs, na.rm = TRUE),
    Correlation = cor(obs, pred, use = "complete.obs")
  )
}


cv_binomial_metrics <- function(obs, pred) {
  eps <- 1e-15
  pred <- pmax(pmin(pred, 1 - eps), eps)
  logloss <- -mean( obs * log(pred) +(1 - obs) * log(1 - pred))
  brier <- mean((obs - pred)^2)
  data.frame( LogLoss = logloss,Brier = brier )
}

cv_count_metrics <- function(obs, pred) {
    pred <- pmax(pred, 1e-10)
    deviance <- 2 * sum(ifelse(obs == 0,-pred,obs * log(obs / pred) - (obs - pred)))
    pbias = 100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)
    data.frame( Deviance = deviance,Pbias = pbias )
}

cv_tweedie_metrics <- function(obs, pred) {
    pred <- pmax(pred, 1e-10)
   deviance <- 2 * sum(  ifelse(obs == 0, -pred,obs * log(obs / pred) - (obs - pred)),na.rm = TRUE)
   pbias = 100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)
   data.frame( Tweedie_Deviance = deviance,
              R2 = 1 - sum((obs - pred)^2, na.rm = TRUE) /sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE),
              Pbias = pbias)
    }


cv_delta_metrics <- function(obs, pred, p_occ = NULL) {
    ii <- obs > 0
    out <- data.frame(Positive_MAE =if(any(ii))  Metrics::mae(obs[ii], pred[ii])  else NA,
                      Positive_RMSE = if(any(ii)) Metrics::rmse(obs[ii], pred[ii])  else NA,
                      Observed_Zero_Rate =mean(obs == 0),
                      Predicted_Zero_Rate =  mean(pred < 1e-5) ,
                      pbias = 100 * sum(pred - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)
                        )
      if(!is.null(p_occ)) {
                      eps <- 1e-15
                      p_occ <- pmax(pmin(p_occ, 1 - eps), eps)
                      obs_occ <- as.integer(obs > 0)
                      out$Occurrence_LogLoss <- -mean(obs_occ * log(p_occ) +(1 - obs_occ) * log(1 - p_occ))
                      out$Occurrence_Brier <- mean( (obs_occ - p_occ)^2)
      out$Occurrence_AUC <- tryCatch( as.numeric(  pROC::auc(obs_occ, p_occ)),error = function(e) NA)
        }
    out
    }


cv_zeroinfl_metrics <- function(obs,pred, p_zero = NULL) {
    out <- data.frame(Zero_Rate_Observed =  mean(obs == 0),
                      Zero_Rate_Predicted = mean(pred < 1e-10))
    if(!is.null(p_zero)) {
      eps <- 1e-15
      p_zero <- pmax(   pmin(p_zero, 1 - eps),eps)
      obs_zero <- as.integer(obs == 0)
      out$Zero_LogLoss <- -mean(obs_zero * log(p_zero) + (1 - obs_zero) * log(1 - p_zero))
      out$Zero_Brier <- mean( (obs_zero - p_zero)^2 )
      out$Zero_AUC <- tryCatch( as.numeric(pROC::auc(obs_zero, p_zero)),error = function(e) NA)
      }
  
  out
  }

dss <- function(y, mu, sigma2) {
  mean(((y - mu)^2 / sigma2) +log(sigma2))
       }


get_cv_scores <- function(dat, family_name) {
   out <- data.frame(
             MAE = Metrics::mae(dat$obs, dat$pred),
            RMSE = Metrics::rmse(dat$obs, dat$pred),
            Bias = mean(dat$pred - dat$obs, na.rm = TRUE)
            )
  
  if (family_name == "binomial") {
    out <- cbind( out,cv_binomial_metrics(dat$obs, dat$pred))
  }
  
  if (family_name %in% c("poisson","nbinom1", "nbinom2")) {
    out <- cbind(   out,  cv_count_metrics(dat$obs, dat$pred) )
  }
  
  if (family_name == "tweedie") {
    out <- cbind( out,cv_tweedie_metrics(dat$obs, dat$pred) )
    }
 if (any(family_name == "binomial") &&
       any(family_name %in% c("tweedie", "Gamma", "lognormal","truncated_nbinom2"))) {
     
     out <- cbind(
       out,
       cv_delta_metrics(dat$obs, dat$pred)
     )
   }
   
  if (grepl("zi|zero", family_name, ignore.case = TRUE)) {
    out <- cbind( out,cv_zeroinfl_metrics(dat$obs, dat$pred))
  }
  
  out
}

columns <- c("Model","Formula","Time-varying","Family", "AIC","cAIC", "EDF_ep","EDF_om","rho","Matern range", "Spatial SD",  "Hessian_positive","Sum loglik")

