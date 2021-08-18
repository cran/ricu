## ---- setup, include = FALSE----------------------------------------------
source(system.file("extdata", "vignettes", "helpers.R", package = "ricu"))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ricu)

## ---- assign-src, echo = FALSE--------------------------------------------
src  <- "mimic_demo"

## ---- assign-demo, echo = FALSE-------------------------------------------
demo <- c(src, "eicu_demo")

## ---- demo-miss, echo = FALSE, eval = !srcs_avail(demo), results = "asis"----
#  demo_missing_msg(demo, "ricu.html")
#  knitr::opts_chunk$set(eval = FALSE)

## ---- eval = FALSE--------------------------------------------------------
#  install.packages("ricu")

## ---- eval = FALSE--------------------------------------------------------
#  remotes::install_github("eth-mds/ricu")

## ---- echo = FALSE, eval = TRUE, results = "asis"-------------------------
cat(
  "```r\n",
  "install.packages(\n",
  "  c(", paste0("\"", sub("_", ".", demo), "\"", collapse = ", "), "),\n",
  "  repos = \"https://eth-mds.github.io/physionet-demo\"\n",
  ")\n",
  "```\n",
  sep = ""
)

## ---- load-ts-------------------------------------------------------------
src  <- "mimic_demo"
demo <- c(src, "eicu_demo")

head(explain_dictionary(src = demo))
load_concepts("alb", src, verbose = FALSE)

## ---- load-id-------------------------------------------------------------
(dat <- load_concepts("height", src, verbose = FALSE))
head(tmp <- as.data.frame(dat, by_ref = TRUE))
identical(dat, tmp)

## ---- load-mult-----------------------------------------------------------
load_concepts("weight", demo, verbose = FALSE)

## ---- create-concept, eval = srcs_avail("mimic_demo")---------------------
ldh <- concept("ldh",
  item("mimic_demo", "labevents", "itemid", 50954),
  description = "Lactate dehydrogenase",
  unit = "IU/L"
)
load_concepts(ldh, verbose = FALSE)

## ---- eval = FALSE--------------------------------------------------------
#  load_concepts("ldh", "mimic_demo")

