% Generated by roxygen2 (4.0.2): do not edit by hand
\name{predict.gbdt}
\alias{predict.gbdt}
\title{Predict classifications from a Gradient Boosting Decision Tree model}
\usage{
\method{predict}{gbdt}(object, n.trees = object$best.iter, ...)
}
\arguments{
\item{object}{a gbdt object, from fit.gbdt}

\item{n.trees}{number of trees used in the prediction.}

\item{...}{passed to \code{predict.gbm}}
}
\description{
Predict classifications from a Gradient Boosting Decision Tree model
}
\examples{
data(sirena)
head(sirena)
m <- fit.gbdt(x=sirena, n.trees=3, shrinkage=0.01, interaction.depth=1,
              n.cores=1)
# NB: The arguments values are caricatural here for the example to run
#     quickly enough and because the data is easy.
# NB: n.cores = 1 is necessary for examples to run on all machines. Feel
#     free to remove it and use more cores on your machine.
pred <- predict(m, newdata=sirena)
head(pred)
}
\seealso{
\code{predict.gbm} in package \code{gbm}
}

