#' Feature-based subsampling of signals
#'
#' Perform a partition of signals based on their characteristics and pick signals homogeneously across groups. This increases the probability of picking rare and different signals, which means that signals in the subsample are more representative of all signal types in the original data.
#'
#' @param x data.frame/matrix of signals
#' @param p proportion of the total number of signals to subsample, between 0 and 1
#' @param k number of groups
#'
#' @return
#' A data.frame containing picked and not-picked signals
#'
#' @seealso \code{\link{plot.subsample}}, \code{\link{subsample_file}}
#'
#' @examples
#' data(sirena)
#' head(sirena)
#' # The data contains:
#' # - signals as lines
#' # - signals features (V1->V8) and type as columns
#' # To take a feature-based subsample of signals, we need to remove
#' # the last column (i.e. the signal type)
#' sub <- subsample(sirena[,-ncol(sirena)], p=0.2)
#' head(sub)
#'
#' @importFrom plyr ddply
#' @export
subsample <- function(x, p, k=10) {

  # check arguments
  if ( missing(x) ) {
    stop("data.frame required for x")
  }
  if ( missing(p) ) {
    stop("Need to specify a proportion for p, between 0 and 1")
  }
  if ( p < 0 | p > 1 ) {
    stop("The proportion of signals subsampled, p, needs to be between 0 and 1")
  }
  if ( k > 50 ) {
    warning(k, " is a *lot* of groups. Consider lowering k")
  }

  # compute the partition
  dist <- dist(scale(x), "euclidian")
  part <- hclust(dist, method="ward.D")
  x$cluster <- cutree(part, k=k)

  # compute the number of element to sample in each partition stratum
  n <- round(nrow(x) * p / k)

  # add original line number, to return the result in the same order as the original
  x$lineno <- 1:nrow(x)

  # extract n element in each stratum
  picks <- ddply(x, ~ cluster, function(X, n) {
    idx <- sample.int(nrow(X), min(n, nrow(X)))
    X$picked <- FALSE
    X$picked[idx] <- TRUE
    return(X)
  }, n=n)
  # reorder the picked data as the original
  picks <- picks[order(picks$lineno),]
  picks <- picks[,!names(picks) %in% "lineno"]

  class(picks) <- c("subsample", "data.frame")

  return(picks)
}

#' Plot subsampled partition
#'
#' @param x data.frame output by the \code{subsample()} function
#' @param ... passed to and from other methods
#'
#' @seealso \code{\link{subsample}}
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_path aes facet_wrap scale_colour_manual scale_x_continuous
#' @export
plot.subsample <- function(x, ...) {
  x <- x[order(x$picked),]
  x$id <- 1:nrow(x)
  # NB: give ids AFTER ordering to get picked signals on top

  # reformat the data for facetting
  xm <- melt(x, id.vars=c("id", "cluster", "picked"), variable.name="characteristic")
  xm$characteristic <- as.numeric(xm$characteristic)

  # plot
  p <- ggplot(xm) +
    geom_path(aes(x=characteristic, y=value, group=id, colour=picked)) +
    facet_wrap( ~ cluster, scales="free_y") +
    scale_colour_manual("", values=c("grey", "black"), labels=c("all data", "subsample")) +
    scale_x_continuous(breaks=sort(unique(xm$characteristic)), minor_breaks=NULL)
  return(p)
}

#' Feature-based subsampling of signals from a file
#'
#' Read a tab or space delimited file containing signal characteristics and subsample a given proportion of signals
#'
#' @param file path to a tab or space delimited file containing signal characteristics
#' @inheritParams subsample
#' @param plot wether to plot the result of the subsampling
#'
#' @return
#' Two files, one for the signals picked in the subsample and one for the non-picked signals
#'
#' @seealso \code{\link{subsample}}
#'
#' @examples
#' # get an example file from within the package
#' data_file <- system.file("sirena.txt", package="soundclass")
#' # check what it looks like
#' file.show(data_file)
#' # copy it to a temporary directory in which we can work
#' tmp <- tempdir()
#' file.copy(data_file, tmp)
#' # perform feature based subsampling on this file and show the results
#' subsample_file(paste0(tmp, "/sirena.txt"), p=0.2)
#' list.files(tmp)
#'
#' @importFrom stringr str_c str_replace
#' @importFrom ggplot2 ggsave
#' @importFrom tools file_ext
#' @export
subsample_file <- function(file, p, k=10, plot=TRUE) {
  # read file
  if ( ! file.exists(file)) {
    stop("Cannot find file ", file)
  }
  x <- read.table(file)

  # subsample and plot the result
  s <- subsample(x, p=p, k=k)
  if ( plot ) {
    pSub <- plot.subsample(s)
    print(pSub)
  }

  # save the results to files
  ext <- file_ext(file)
  base <- str_replace(file, str_c("\\.", ext, "$"), "")

  picked <- subset(s, subset=s$picked, select=c(-cluster, -picked))
  not_picked <- subset(s, subset=!s$picked, select=c(-cluster, -picked))

  write.table(picked,     file=str_c(base, "-subsample.txt"), sep="\t", row.names=FALSE, col.names=FALSE)
  write.table(not_picked, file=str_c(base, "-rest.txt"),      sep="\t", row.names=FALSE, col.names=FALSE)
  if ( plot ) { ggsave(pSub, filename=str_c(base, "-subsample_plot.pdf"), width=8, height=5) }

  return(invisible(s))
}
