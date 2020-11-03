## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ricu)
library(ggplot2)

on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

create_filename <- function(base_dir, src) {
  paste0(file.path(base_dir, "extdata", "vignettes", "uom", src), ".rds")
}

load_concepts <- function(concepts, src, ...) {

  cached_file <- create_filename(system.file(package = "ricu"), src)

  if (file.exists(cached_file) && on_cran()) {
    return(readRDS(cached_file))
  }

  res <- ricu::load_concepts(concepts, src, ...)
  dst <- create_filename(file.path("..", "inst"), src)

  if (!dir.exists(dirname(dst))) {
    dir.create(dirname(dst), recursive = TRUE)
  }

  saveRDS(res, dst, version = 2L)

  res
}

## ----concept-range------------------------------------------------------------
data_src <- c("mimic_demo", "eicu_demo")

concepts <- c(
  "map", "lact" , "crea", "bili", "plt"
)

dat <- lapply(data_src,
  function(src) load_concepts(concepts, src, verbose = FALSE)
)

names(dat) <- data_src

dat

## ----uom, echo = FALSE, fig.width = 6-----------------------------------------
take_quants <- function(x, lwr, upr) {
  qq <- quantile(x, probs = c(lwr, upr), na.rm = TRUE)
  x[!is.na(x) & x >= qq[1L] & x <= qq[2L]]
}

for (conc in concepts) {

  feat <- lapply(dat, `[[`, conc)
  meds <- vapply(feat, median, numeric(1L), na.rm = TRUE)
  feat <- Map(data.frame, src = names(feat),
              dat = lapply(feat, take_quants, lwr = 0.05, upr = 0.95))

  title <- paste0(conc, ": median values ",
    paste0(round(meds, 2), " (", names(meds), ")", collapse = ", ")
  )

  print(
    ggplot(Reduce(rbind, feat), aes(x = dat, fill = src)) +
      geom_density(alpha=0.5) +
      xlab(conc) + theme_bw() + ggtitle(title)
  )
}

