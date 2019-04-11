#river conditions report utils

#define functions

#import library
library(zoo)

calc_fdd <- function(x) {
  x <- na.approx(x)
  neg_temp <- case_when(
    x <= 0 ~ x,
    x > 0 ~ 0,
    is.na(x) ~ 0)
  fdd <- abs(cumsum(neg_temp))
}

calc_pdd <- function(x) {
  x <- na.approx(x)
  pos_temp <- case_when(
    x >= 0 ~ x,
    x < 0 ~ 0,
    is.na(x) ~ 0)
  pdd <- cumsum(pos_temp)
}
