#' Fit a Gradient Boosting Decision Tree model
#' 
#' @param x a data.frame with signal characteristics in the first columns and signal type (classification) in the last column
#' @param n.trees see \code{?gbm}
#' @param shrinkage see \code{?gbm}
#' @param interaction.depth see \code{?gbm}
#' @param ... passed to \code{gbm}
#'
#' @seealso \code{gbm}, \code{summary.gbm}, \code{predict.gbm} in package \code{gbm}
#'
#' @examples
#' data(sirena)
#' head(sirena)
#' m <- fit.gbdt(x=sirena, n.trees=3, shrinkage=0.01, interaction.depth=1)
#' # NB: the parameters are caricatural here
#' #     for the example to run quickly enough and because the data is easy
#' print(m)
#' summary(m)
#' pred <- predict(m, newdata=sirena)
#' head(pred)
#' (cm <- confusion_matrix(true=sirena$type, pred=pred$type))
#' confusion_stats(cm)
#'
#' @importFrom stringr str_c
#' @import gbm
#' @export
fit.gbdt <- function(x, n.trees=5000, shrinkage=0.001, interaction.depth=3, ...) {

  # prepare model formula
  type <- names(x)[ncol(x)]
  vars <- setdiff(names(x), type)
  
  formula <- str_c(type, " ~ ", str_c(vars, collapse=" + "))
  formula <- formula(formula)

  # fit model
  m <- gbm(
    formula=formula,
    data=x,
    distribution="multinomial",
    n.trees=n.trees,      # should be > 1000 to be robust
    shrinkage=shrinkage,  # should be small to allow enough trees
    cv.folds=5,           # allows estimating error in prediction and then use gbm.perf with method cv to find optimal n.trees
    interaction.depth=interaction.depth,  
                          # higher level interactions means faster fit => decrease shrink/learning rate to compensate
    ...
  )

  # find optimal n.trees
  m$best.iter <- gbm.perf(m, method="cv", plot.it=FALSE)

  # sub class the result
  class(m) <- c("gbdt", class(m))

  return(m)
}

#' Predict classifications from a Gradient Boosting Decision Tree model
#' 
#' @param object a gbdt object, from fit.gbdt
#' @param n.trees number of trees used in the prediction.
#' @param ... passed to \code{predict.gbm}
#'
#' @seealso \code{predict.gbm} in package \code{gbm}
#'
#' @examples
#' data(sirena)
#' head(sirena)
#' m <- fit.gbdt(x=sirena, n.trees=3, shrinkage=0.01, interaction.depth=1)
#' pred <- predict(m, newdata=sirena)
#' head(pred)
#'
#' @import gbm
#' @export
predict.gbdt <- function(object, n.trees=object$best.iter, ...) {
  # predict probabilities of being of each type
  probas <- predict.gbm(object, type="response", n.trees=n.trees, ...)[,,1]
  
  # extract predicted type
  predictedType <- colnames(probas)[apply(probas, 1, which.max)]
  
  # put both in the same data.frame
  # TODO choose between one the other and the two
  out <- as.data.frame(probas)
  out$type <- predictedType
  
  return(out)
}

#' Fit GBDT model and predict classification
#' 
#' @param data a data.frame with signal characteristics of signals to be predicted
#' @param train a data.frame with signal characteristics and classification of signals from which to learn the model
#' @param ... passed to \code{fit.gbdt}
#'
#' @examples
#' data(sirena)
#' sub <- subsample(sirena[,-ncol(sirena)], p=0.2)
#' train <- sirena[sub$picked,]
#' data <- sirena[!sub$picked,]
#' pred <- classify(data=data[,-ncol(data)], train=train, n.trees=100, shrinkage=0.01, n.minobsinnode=1)
#' head(pred)
#' (cm <- confusion_matrix(true=data$type, pred=pred$type))
#' confusion_stats(cm)
#'
#' @export
classify <- function(data, train, ...) {
  # check arguments existence
  if ( missing(data) ) {
    stop("Need to provide a dataset in data")
  }
  if ( missing(train) ) {
    stop("Need to provide a training set in train")
  }
  # check column names
  type <- names(train)[ncol(train)]
  varsTrain <- setdiff(names(train), type)
  varsData <- names(data)
  if ( ! all(c(varsTrain %in% varsData, varsData %in% varsTrain)) ) {
    stop("Columns mismatch between training set and data")
  }
  
  m <- fit.gbdt(x=train, ...)
  
  pred <- predict(m, newdata=data)
  
  return(pred)
}

#' Fit GBDT model and predict classification
#' 
#' @param data a tab or space delimited file with signal characteristics of signals to be predicted
#' @param train a tab or space delimited file with signal characteristics and classification of signals from which to learn the model
#' @param ... passed to \code{fit.gbdt}
#'
#' @return
#' A file with the original data and the predictions (probabilities and predicted type)
#' 
#' @seealso \code{\link{classify}} for an example of usage
#'
#' @export
classify_file <- function(data, train, ...) {
  if ( ! file.exists(data)) {
    stop("Cannot find file ", file)
  }
  dataD <- read.table(data)
  if ( ! file.exists(train)) {
    stop("Cannot find file ", train)
  }
  trainD <- read.table(train)
  
  pred <- classify(data=dataD, train=trainD, ...)

  out <- cbind(data, pred)

  # save the results to files
  ext <- file_ext(data)
  base <- str_replace(data, str_c("\\.", ext, "$"), "")
  
  write.table(out, file=str_c(base, "-classified.txt"), sep="\t", row.names=FALSE, col.names=FALSE)
  # if ( plot ) { ggsave(pSub, filename=str_c(base, "-subsample_plot.pdf"), width=8, height=5) }

  return(invisible(out))
}

