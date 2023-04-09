## ----setup, include = FALSE-----------------------------------------------
options(
  width = 76,
  kableExtra.latex.load_packages = FALSE,
  crayon.enabled = FALSE
)

library(ricu)
library(data.table)
library(forestmodel)
library(survival)
library(ggplot2)
library(kableExtra)

source(system.file("extdata", "vignettes", "helpers.R", package = "ricu"))

srcs <- c("mimic", "eicu", "aumc", "hirid", "miiv")

## ---- assign-src, echo = FALSE--------------------------------------------
src  <- "mimic_demo"

## ---- assign-demo, echo = FALSE-------------------------------------------
demo <- c(src, "eicu_demo")

## ---- demo-miss, echo = FALSE, eval = !srcs_avail(demo), results = "asis"----
#  demo_missing_msg(demo, "ricu.pdf")
#  knitr::opts_chunk$set(eval = FALSE)

## ----demo-data, eval = FALSE----------------------------------------------
#  install.packages(
#    c("mimic.demo", "eicu.demo"),
#    repos = "https://eth-mds.github.io/physionet-demo"
#  )

## ----mimic-tbls, eval = TRUE----------------------------------------------
mimic

## ----width-inc, include = FALSE---------------------------------------------
old_width <- options(width = 78)[["width"]]

## ----eicu-tbls, eval = TRUE-------------------------------------------------
eicu

## ----width-dec, include = FALSE-------------------------------------------
options(width = old_width)

## ----hirid-tbls, eval = TRUE----------------------------------------------
hirid

## ----aumc-tbls, eval = TRUE-----------------------------------------------
aumc

## ----miiv-tbls, eval = TRUE-----------------------------------------------
miiv

## ----src-overview, echo = FALSE, results = "asis", cache = TRUE-----------
as_quant <- function(x) {

  if (is_id_tbl(x)) {
    x <- data_col(x)
  }

  if (identical(length(x), 0L) || isTRUE(is.na(x))) {
    return("-")
  }

  res <- format_2(quantile(x, probs = seq(0.25, 0.75, 0.25), na.rm = TRUE))

  paste0(res[2L], " (", res[1L], "--", res[3L], ")")
}

big_mark <- function(x) {

  if (identical(length(x), 0L) || isTRUE(is.na(x))) {
    return("-")
  }

  formatC(x, big.mark = ",", format = "d")
}

format_2 <- function(x) {
  formatC(x, digits = 2L, format = "f")
}

n_patient <- function(x, type) {
  if (type %in% names(as_id_cfg(x))) nrow(stay_windows(x, type)) else NA
}

feat_freq <- function(src, concept, time_span = "hours") {
  res <- load_concepts(concept, src, interval = mins(1L), verbose = FALSE)
  res <- res[, 1 / diff(as.double(get(index_var(res)), units = time_span)),
             by = c(id_var(res))]
  res
}

years <- function(src) {
  switch(src,
    mimic = "2001--2012",
    eicu = "2014--2015",
    hirid = "2008--2016",
    aumc = "2003--2016",
    miiv = "2008--2019",
    NA
  )
}

country <- function(src) {
  switch(src,
    mimic = "United States",
    eicu = "United States",
    hirid = "Switzerland",
    aumc = "Netherlands",
    miiv = "United States",
    NA
  )
}

summarize <- function(src, avail) {

  ids <- as_id_cfg(src)
  cnc <- avail[, src]

  nrow(stay_windows(src, "icustay"))

  los_icu <- load_concepts("los_icu", src, verbose = FALSE)

  hosp_len <- if ("hadm" %in% names(ids)) {
    load_concepts("los_hosp", src, id_type = "hadm", verbose = FALSE)
  }

  fil <- list.files(src_data_dir(src), recursive = TRUE, full.names = TRUE)
  siz <- sum(vapply(fil, file.size, numeric(1L))) * 1e-9
  row <- vapply(as_src_env(src), nrow, integer(1L))

  c(`Number of tables` = big_mark(length(as_src_env(src))),
    `Disk storage [GB]` = format_2(siz),
    `Largest table [rows]` = big_mark(max(row)),
    `Available concepts` = sum(cnc),
    `Time span` = years(src),
    `Country of origin` = country(src),
    `ICU` = big_mark(n_patient(src, "icustay")),
    `Hospital` = big_mark(n_patient(src, "hadm")),
    `Unique patients` = big_mark(n_patient(src, "patient")),
    `ICU stays` = as_quant(los_icu),
    `Hospital stays` = as_quant(hosp_len),
    `Heart rate` = as_quant(feat_freq(src, "hr")),
    `Mean arterial pressure` = as_quant(feat_freq(src, "map")),
    `Bilirubin` = as_quant(feat_freq(src, "bili", "days")),
    `Lactate` = as_quant(feat_freq(src, "lact", "days"))
  )
}

if (srcs_avail(demo) && (!srcs_avail(srcs) || quick_build())) {
  srcs <- demo
}

src_names <- c(
  mimic = "MIMIC-III", eicu = "eICU", hirid = "HiRID", aumc = "AmsterdamUMCdb",
  miiv = "MIMIC-IV", mimic_demo = "MIMIC (demo)", eicu_demo = "eICU (demo)"
)[srcs]

src_names[is.na(src_names)] <- srcs[is.na(src_names)]

dict <- load_dictionary(srcs)
avai <- concept_availability(dict, include_rec = FALSE)
summ <- vapply(srcs, summarize, character(15L), avai)

colnames(summ)     <- src_names
rownames(summ)     <- rownames(summ)
rownames(summ)[4L] <- paste0(rownames(summ)[4L],
                             footnote_marker_symbol(1, "latex"))

n_rec_cpt <- nrow(concept_availability(dict, include_rec = TRUE)) -
             nrow(avai)

capt <- paste(
  "Comparison of datasets supported by \\pkg{ricu}, highlighting some of",
  "the major similarities and distinguishing features among the five data",
  "sources described in the preceding sections. Values followed by",
  "parenthesized ranges represent medians and are accompanied by",
  "interquartile ranges."
)

tbl <- kable(summ, format = "latex", escape = FALSE, booktabs = TRUE,
             caption = capt, label = "datasets")
tbl <- pack_rows(tbl, "Data collection", 5, 6)
tbl <- pack_rows(tbl, "Admission counts", 7, 9)
tbl <- pack_rows(tbl, "Stay lengths [day]", 10, 11)
tbl <- pack_rows(tbl, "Vital signs [1/hour]", 12, 13)
tbl <- pack_rows(tbl, "Lab tests [1/day]", 14, 15)
tbl <- footnote(tbl, symbol = paste(
  "These values represent the number of atomic concepts per data source.",
  "Additionally,", n_rec_cpt, "recursive concepts are available, which",
  "build on data source specific atomic concepts in a source agnostic manner",
  "(see Section \\\\ref{concept-specification} for details)."),
  threeparttable = TRUE, escape = FALSE
)

if (identical(srcs, demo)) {
  tbl
} else {
  landscape(tbl)
}

## ---- full-miss, echo = FALSE, eval = identical(srcs, demo), results = "asis"----
#  demo_instead_full_msg(demo, srcs, "ricu.pdf")

## ---- load-conc, eval = srcs_avail(demo)----------------------------------
src  <- "mimic_demo"
demo <- c(src, "eicu_demo")

load_concepts("hr", demo, verbose = FALSE)

## ----id-tbl, eval = TRUE--------------------------------------------------
(dat <- ts_tbl(a = 1:5, b = hours(1:5), c = rnorm(5)))
dat[["b"]] <- dat[["b"]] + mins(30)
dat

## ----win-tbl, eval = TRUE-------------------------------------------------
(dat <- win_tbl(a = 1:5, b = hours(1:5), c = mins(rep(90, 5)),
                d = runif(5)))
expand(dat)

## ---- load-dict, eval = srcs_avail(demo)----------------------------------
dict <- load_dictionary(demo)
head(dict)
explain_dictionary(head(dict))

## ---- dict-cat, eval = srcs_avail(demo)-----------------------------------
table(vapply(dict, `[[`, character(1L), "category"))

## ---- load-phys, eval = srcs_avail(src)-----------------------------------
load_concepts(c("alb", "glu"), src, interval = mins(15L),
              verbose = FALSE)

## ---- load-demog, eval = srcs_avail(src)----------------------------------
load_concepts(c("age", "glu"), src, verbose = FALSE)

## ---- load-treat, eval = srcs_avail(src)----------------------------------
load_concepts(c("abx", "vent_ind", "norepi_rate", "norepi_dur"), src,
              verbose = FALSE)

## ---- load-dex, eval = srcs_avail("mimic_demo")---------------------------
load_concepts("dex", "mimic_demo", verbose = FALSE)

## ---- load-out, eval = srcs_avail(src)------------------------------------
load_concepts(c("sirs", "death"), src, verbose = FALSE,
              keep_components = TRUE)

## ----mimic-adm, eval = srcs_avail("mimic_demo")---------------------------
mimic_demo$admissions

## ----mimic-sub, eval = srcs_avail("mimic_demo")---------------------------
subset(mimic_demo$admissions, subject_id > 44000, language:ethnicity)

## ----mimic-tidy, eval = srcs_avail("mimic_demo")--------------------------
subject_id <- 44000:45000
subset(mimic_demo$admissions, .data$subject_id %in% .env$subject_id,
       subject_id:dischtime)

## ----load-src, eval = srcs_avail("mimic_demo")----------------------------
load_src(mimic_demo$admissions, subject_id > 44000,
         cols = c("hadm_id", "admittime", "dischtime"))

## ----load-dt, eval = FALSE------------------------------------------------
#  load_difftime(mimic_demo$admissions, subject_id > 44000,
#                cols = c("hadm_id", "admittime", "dischtime"))

## ----load-dt-print, eval = srcs_avail("mimic_demo"), echo = FALSE---------
load_difftime(mimic_demo$admissions, subject_id > 44000,
              cols = c("hadm_id", "admittime", "dischtime"))[]

## ----load-id, eval = FALSE------------------------------------------------
#  load_id(mimic_demo$admissions, subject_id > 44000,
#          cols = c("admittime", "dischtime"), id_var = "hadm_id")

## ----load-id-print, eval = srcs_avail("mimic_demo"), echo = FALSE---------
load_id(mimic_demo$admissions, subject_id > 44000,
        cols = c("admittime", "dischtime"), id_var = "hadm_id")[]

## ----id-win, eval = srcs_avail("mimic_demo")------------------------------
id_windows(mimic_demo)

## ----id-orig, eval = srcs_avail("mimic_demo")-----------------------------
id_origin(mimic_demo, "icustay_id")

## ----mimic-cfg, eval = TRUE-----------------------------------------------
cfg <- load_src_cfg("mimic_demo")
str(cfg, max.level = 3L, width = 70L)
mi_cfg <- cfg[["mimic_demo"]]

## ----mimic-ids, eval = TRUE-----------------------------------------------
as_id_cfg(mi_cfg)

## ----mimic-col, eval = TRUE-----------------------------------------------
as_col_cfg(mi_cfg)

## ----mimic-tbl, eval = TRUE-----------------------------------------------
as_tbl_cfg(mi_cfg)

## ---- ext-difftime--------------------------------------------------------
ms_as_min <- function(x) {
  as.difftime(as.integer(x / 6e4), units = "mins")
}

aumc_difftime <- function(x, rows, cols = colnames(x),
                          id_hint = id_vars(x),
                          time_vars = ricu::time_vars(x), ...) {

  if (id_hint %in% colnames(x)) {
    id_sel <- id_hint
  } else {
    id_opt <- id_var_opts(sort(as_id_cfg(x), decreasing = TRUE))
    id_sel <- intersect(id_opt, colnames(x))[1L]
  }

  stopifnot(is.character(id_sel), length(id_sel) == 1L)

  if (!id_sel %in% cols) {
    cols <- c(id_sel, cols)
  }

  time_vars <- intersect(time_vars, cols)

  dat <- load_src(x, {{ rows }}, cols)
  dat <- dat[, c(time_vars) := lapply(.SD, ms_as_min),
             .SDcols = time_vars]

  as_id_tbl(dat, id_vars = id_sel, by_ref = TRUE)
}

## ---- ext-win-------------------------------------------------------------
aumc_windows <- function(x) {

  ids <- c("admissionid", "patientid")
  sta <- c("admittedat", "firstadmittedat")
  end <- c("dischargedat", "dateofdeath")

  tbl <- as_src_tbl(x, "admissions")

  res <- tbl[, c(ids, sta[1L], end)]
  res <- res[, c(sta[2L]) := 0L]
  res <- res[, c(sta, end) := lapply(.SD, ms_as_min),
             .SDcols = c(sta, end)]

  res <- data.table::setcolorder(res, c(ids, sta, end))
  res <- rename_cols(res, c(ids, paste0(ids, "_start"),
                                 paste0(ids, "_end")), by_ref = TRUE)

  as_id_tbl(res, ids[2L], by_ref = TRUE)
}

## ----cox-surv, eval = srcs_avail("mimic_demo")----------------------------
src <- "mimic_demo"

cohort <- load_id("icustays", src, dbsource == "metavision",
                  cols = NULL)
cohort <- load_concepts("age", src, patient_ids = cohort,
                        verbose = FALSE)

dat <- load_concepts(c("lact", "death", "sofa"), src,
                     patient_ids = cohort[age > 20 & age < 90, ],
                     verbose = FALSE)

dat <- dat[,
  head(.SD, n = match(TRUE, death, .N)), by = c(id_vars(dat))
]

dat <- fill_gaps(dat)

dat <- replace_na(dat, c(NA, FALSE), type = c("locf", "const"),
                  by_ref = TRUE, vars = c("lact", "death"),
                  by = id_vars(dat))

cox_mod <- coxph(
  Surv(charttime - 1L, charttime, death) ~ lact + sofa,
  data = dat
)

## ----cox-plot, eval = srcs_avail(src), echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8----
theme_fp <- function(...) {
  theme_bw(...) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

forest_model(cox_mod, theme = theme_fp(16))

## ----ins24, eval = srcs_avail(src)----------------------------------------
ins_breaks <- c(0, 1, 10, 20, 40, Inf)

ins_cb <- function(ins, ...) {

  day_one <- function(x) x >= hours(0L) & x <= hours(24L)

  idx_var <- index_var(ins)
  ids_var <- id_vars(ins)

  ins <- ins[
    day_one(get(idx_var)), list(ins24 = sum(ins)), by = c(ids_var)
  ]

  ins <- ins[,
    ins24 := list(cut(ins24, breaks = ins_breaks, right = FALSE))
  ]

  ins
}

ins24 <- load_dictionary(src, "ins")
ins24 <- concept("ins24", ins24, "insulin in first 24h",
                 aggregate = "sum", callback = ins_cb,
                 target = "id_tbl", class = "rec_cncpt")

## ----diab, eval = srcs_avail(src)-----------------------------------------
grep_diab <- function(x) {
  grepl("^250\\.?[0-9]{2}$", x)
}

diab  <- item(src, table = "diagnoses_icd",
              callback = transform_fun(grep_diab),
              class = "col_itm")

diab  <- concept("diab", diab, "diabetes", target = "id_tbl",
                 class = "lgl_cncpt")

dat <- load_concepts(c(ins24, diab), id_type = "icustay",
                     verbose = FALSE)
dat <- replace_na(dat, "[0,1)", vars = "ins24")

dat

## ----diabetes-visualize, echo = FALSE, eval = srcs_avail(src), fig.height = 3----
dat <- dat[, weight := 1 / .N, by = diab]
ggplot(dat, aes(x = ins24, fill = diab)) +
  stat_count(aes(weight = weight), alpha = 0.75, position = "dodge") +
  labs(x = "Amount of administered insulin in first 24h of ICU stay [units]",
       y = "Proportion of patients",
       fill = "Diabetic") +
  theme_bw(10)

## ----session-info, include = FALSE----------------------------------------
sessionInfo()

