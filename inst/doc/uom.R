## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ricu)
library(ggplot2)

## ---- echo = FALSE------------------------------------------------------------
demo  <- c("mimic_demo", "eicu_demo")
full  <- c("mimic", "eicu", "aumc", "hirid")

avail <- is_data_avail(c(demo, full))

srcs_avail <- function(x) all(avail[x])

if (!srcs_avail(demo)) {

  msg <- paste(
    "Note: Examples in this vignette require that one or more of datasets",
    paste0("`", demo, "`", collapse = ", "), "are available. Chunks that",
    "depend on certain datasets will not be evaluated if the corresponding",
    "dataset is missing. In order to download data, have a look at",
    "`?setup_src_data`."
  )
  msg <- paste(strwrap(msg), collapse="\n")
  message(msg)
}

## ---- eval = srcs_avail(demo)-------------------------------------------------
(dat <- load_concepts(c("lact", "map"), c("mimic_demo", "eicu_demo")))

## ---- echo = FALSE, fig.width = 6, eval = sum(avail) >= 2L--------------------
filter_quants <- function(x, lwr, upr) {

  do_filter <- function(x) {
    qq <- quantile(x, probs = c(lwr, upr), na.rm = TRUE)
    x[!is.na(x) & x >= qq[1L] & x <= qq[2L]]
  }

  x[, list(val = do_filter(get(data_var(x)))), by = "source"]
}

srcs <- c(full, demo)
srcs <- head(srcs[is_data_avail(srcs)], n = length(full))

concepts <- c("map", "lact" , "crea", "bili", "plt")

dat <- load_concepts(concepts, srcs, verbose = FALSE, merge = FALSE)
dat <- lapply(dat, filter_quants, lwr = 0.025, upr = 0.975)

for (feat in names(dat)) {

  title <- dat[[feat]][, list(val = median(val)), by = "source"]
  title <- paste0(title$val, " (", title$source, ")", collapse = ", ")
  title <- paste0(feat, ": ", title)

  print(
    ggplot(dat[[feat]], aes(x = val, fill = source)) +
      geom_density(alpha = 0.5) +
      xlab(feat) + theme_bw() + ggtitle(title)
  )
}

