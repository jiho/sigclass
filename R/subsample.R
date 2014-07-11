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
#' @importFrom plyr ddply
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
  part <- hclust(dist, method="ward")
  x$cluster <- cutree(part, k=k)

  # compute the number of element to sample in each partition stratum
  n <- round(nrow(x) * p / k)

  # extract n element in each stratum
  picks <- ddply(x, ~ cluster, function(X, n) {
    idx <- sample.int(nrow(X), min(n, nrow(X)))
    X$picked <- FALSE
    X$picked[idx] <- TRUE
    return(X)
  }, n=n)

  class(picks) <- c("subsample", "data.frame")

  return(picks)
}

#' Plot subsampled partition
#'
#' @param x data.frame output by the \code{subsample()} function
#' @param ... passed to and from other methods
#'
#' @seealso \code{\link{subsample}}
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_path aes facet_wrap scale_colour_manual scale_x_continuous
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
#' @importFrom stringr str_c str_replace
#' @importFrom ggplot2 ggsave
#' @importFrom tools file_ext
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
