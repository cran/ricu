## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ricu)

## ---- echo = FALSE------------------------------------------------------------
demo  <- c("mimic_demo", "eicu_demo")
alls  <- demo

avail <- is_data_avail(alls)

srcs_avail <- function(x) all(avail[x])

if (!srcs_avail(alls)) {

  msg <- paste(
    "Note: Examples in this vignette require that one or more of datasets",
    paste0("`", alls, "`", collapse = ", "), "are available. Chunks that",
    "depend on certain datasets will not be evaluated if the corresponding",
    "dataset is missing. In order to download and setup data, have a look at",
    "`?setup_src_data`."
  )
  msg <- paste(strwrap(msg), collapse = "\n")
  message(msg)
}

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("ricu")

## ---- eval = FALSE------------------------------------------------------------
#  remotes::install_github("eth-mds/ricu")

## ---- eval = FALSE------------------------------------------------------------
#  install.packages(
#    c("mimic.demo", "eicu.demo"),
#    repos = "https://eth-mds.github.io/physionet-demo"
#  )

## ---- eval = srcs_avail(demo)-------------------------------------------------
head(explain_dictionary(src = c("mimic_demo", "eicu_demo")))
load_concepts("alb", "mimic_demo", verbose = FALSE)

## ---- eval = srcs_avail("eicu_demo")------------------------------------------
(dat <- load_concepts("height", "eicu_demo", verbose = FALSE))
head(as.data.frame(dat, by_ref = TRUE))

## ---- eval = srcs_avail("mimic_demo")-----------------------------------------
ldh <- concept("ldh",
  item("mimic_demo", "labevents", "itemid", 50954),
  description = "Lactate dehydrogenase",
  unit = "IU/L"
)
load_concepts(ldh, verbose = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  load_concepts("ldh", "mimic_demo")

