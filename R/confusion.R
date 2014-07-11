#' Compute confusion matrix
#'
#' @param true true categories
#' @param pred predicted categories (last column of the data.frame output by \code{predict.gbdt})
#'
#' @export
confusion_matrix <- function(true, pred) {
  # make sure observed and predicted categories are the same
  if (!is.factor(true)) {
    true <- factor(true)
  }
  pred <- factor(pred, levels=levels(true))

  # compute confusion matrix
  cm <- table(list(pred=pred, true=true))

  return(cm)
}


#' Compute confusion statistics (recall, precision, etc.)
#'
#' @param x a confusion matrix (table with predictions as lines and true observations as columns, as output by \code{confusion_matrix})
#'
#' @export
confusion_stats <- function(x) {

  (tp <- diag(x))              # true positive
  (fp <- rowSums(x) - tp)      # false positive
  (fn <- colSums(x) - tp)      # false negative
  (tn <- sum(x) - tp - fp -fn) # true negative

  # store it
  stats <- data.frame(tp, fp, fn, tn)

  # define a formatter for percentages
  format_percent <- function(x, precision=1) {
    round(x * 100, precision)
  }

  # precision = quantify how "pure" the identified signals are
  stats$precision <- format_percent(tp / (tp + fp))

  # recall = capacity to get signals of a given origin
  stats$recall <- format_percent(tp / (tp + fn))

  # F1 score = combination of precision and recall
  # http://en.wikipedia.org/wiki/F1_score
  # the higher the better
  stats$F1 <- with(stats, (2 * precision * recall) / (precision + recall))

  return(stats)
}
