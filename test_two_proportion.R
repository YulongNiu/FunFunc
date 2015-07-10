#####################test two sample proportion########################
##' Test proportions from two samples
##'
##' The "pooled" and "binomial" method are used for calculate the standard deviation (SE). 
##' @title Test proportions from two samples with "pooled" and "binomial" method.
##' @param s1 Observed number of successful events from the first sample.
##' @param n1 Total number of the first sample.
##' @param s2 Observed number of successful events from the second sample.
##' @param n2 Total number of the second sample.
##' @param method "pooled" or "binomial" to calculate the SE.
##' @param tail "two" for two-tailed test. "up" and "down" are for the single-tailed test.
##' @return The list of two proportions, z value, and the p value.
##' @examples
##' ## from the \url{http://www.dummies.com/how-to/content/how-to-compare-two-population-proportions.html}
##' TestPropor(26, 364, 8, 210)
##' TestPropor(26, 364, 8, 210, method = 'binomial')
##' prop.test(c(26, 8), c(364, 210), correct = FALSE)
##' ## from the \url{https://onlinecourses.science.psu.edu/stat414/node/268}
##' TestPropor(351, 605, 41, 195)
##' TestPropor(351, 605, 41, 195, tail = 'up')
##' TestPropor(351, 605, 41, 195, method = 'binomial')
##' prop.test(c(351, 41), c(605, 195), correct = FALSE)
##' ## from \url{http://stattrek.com/hypothesis-test/difference-in-proportions.aspx?Tutorial=AP}
##' TestPropor(0.38 * 100, 100, 0.51 * 200, 200, tail = 'down')
##' ## from \url{http://ww2.coastal.edu/kingw/statistics/R-tutorials/proport.html}
##' @references \url{http://math.arizona.edu/~ghystad/Chapter14.pdf}
##' @author Yulong Niu \email{niuylscu@@gmail.com}
##' @export
##' 
TestPropor <- function(s1, n1, s2, n2, method = 'pooled', tail = 'two') {
  ## calculate SE
  if (method == 'pooled') {
    SE = sqrt((s1 + s2) * (n1 + n2 -s1 - s2) / ((n1 + n2) * n1 * n2))
  }
  else if (method == 'binomial') {
    SE = sqrt(s1 * (n1 - s1) / (n1^3) + s2 * (n2 - s2) / (n2^3))
  }
  else {}

  zVal <- (s1 * n2 - s2 * n1) / (n1 * n2 * SE)

  if (tail == 'two') {
    pvalue <- 2 * pnorm(abs(zVal), mean = 0, sd = 1, lower.tail = FALSE)
  }
  else if (tail == 'up') {
    pvalue <- pnorm(zVal, mean = 0, sd = 1, lower.tail = FALSE)
  }
  else if (tail == 'down') {
    pvalue <- pnorm(zVal, mean = 0, sd = 1)
  }

  returnVal <- list(p1 = s1 / n1,
                    p2 = s2 / n2,
                    Zvalue = zVal,
                    pvalue = pvalue)

  return(returnVal)
}
#######################################################################
