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


#' Plot the distribution of signals in categories after prediction
#'
#' @param true true categories
#' @param pred predicted categories (last column of the data.frame output by \code{predict.gbdt})
#' @param prob for each signal, probabilities to be in each category
#' @param ... passed to \code{geom_??}
#'
#' @seealso \code{gbm}, \code{summary.gbm}, \code{predict.gbm} in package \code{gbm}
#'
#' @examples
#' data(sirena)
#' sub <- subsample(sirena[,-ncol(sirena)], p=0.2)
#' train <- sirena[sub$picked,]
#' data <- sirena[!sub$picked,]
#' p <- classify(data=data[,-ncol(data)], train=train,
#'               n.trees=100, shrinkage=0.01, n.minobsinnode=1,
#'               n.cores=1)
#' confusion_proba_plot(true=data$type, pred=p$type, prob=p[,1:4])
#'
#' @importFrom plyr ddply
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_point geom_line facet_wrap scale_alpha_manual scale_shape_manual labs
#' @export
confusion_proba_plot <- function(true, pred, prob) {

  # checks
  categoriesInTrue <- sort(unique(true))
  categoriesInPred <- sort(unique(pred))

  if ( ! all(categoriesInPred %in% categoriesInTrue) ) {
    warning("Some categories in `true` were never predicted")
  }
  if ( ! all(categoriesInTrue %in% categoriesInPred) ) {
    stop("`pred` has more categories than `true`")
  }

  categories <- union(categoriesInTrue, categoriesInPred)
  if ( ! setequal(names(prob), categories) ) {
    stop("Categories and probabilities columns do not match")
  }

  # assemble all data
  d <- data.frame(prob, true, pred, stringsAsFactors=FALSE)
  d$id <- 1:nrow(d)

  # order by proba of being of the correct type (cleaner plot)
  d <- ddply(d, ~true, function(x) {
    cType <- x$true[1]
    x$rank <- nrow(x) - rank(x[,cType], ties.method="random")
    x$perc <- x$rank / max(x$rank) * 100  # relative rank
    return(x)
  })

  # melt data for plotting
  dm <- melt(d, id.vars=c("id", "true", "pred", "rank", "perc"), variable.name="category", value.name="proba")

  p <- ggplot(dm, aes(x=perc, y=proba, colour=category, )) +
    geom_line(alpha=0.5) +
    geom_point(aes(alpha=category==pred, shape=true==pred)) +
    facet_wrap(~true) +
    scale_alpha_manual(values=c(0.5, 1), guide="none") +
    scale_shape_manual(values=c(4, 16), guide="none") +
    labs(x="Percentage rank", y="Probability", colour="Category", title="Within each true category (panels), probability for each signal to be in each category\n(wrong predictions are highlighted)")

  return(p)
}
