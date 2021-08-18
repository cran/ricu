## ---- setup, include = FALSE----------------------------------------------
source(system.file("extdata", "vignettes", "helpers.R", package = "ricu"))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ricu)
library(ggplot2)

## ---- assign-demo, echo = FALSE-------------------------------------------
demo <- c("mimic_demo", "eicu_demo")

## ---- assign-srcs, echo = FALSE-------------------------------------------
srcs <- c("mimic", "eicu", "aumc", "hirid", "miiv")

## ---- demo-miss, echo = FALSE, eval = !srcs_avail(demo), results = "asis"----
#  demo_missing_msg(demo, "uom.html")
#  knitr::opts_chunk$set(eval = FALSE)

## ---- demo-load, eval = srcs_avail("mimic_demo")--------------------------
(dat <- load_concepts(c("lact", "map"), "mimic_demo"))

## ---- sep-itms, include = FALSE, eval = TRUE------------------------------
assign_id <- function(id, itm, nme) `[[<-`(itm, nme, id)

rep_itm <- function(itm, nme = "ids", fun = identity) {
  as_item(lapply(fun(itm[[nme]]), assign_id, itm, nme))
}

unlst <- function(x) unlist(x, recursive = FALSE, use.names = FALSE)

split_ind <- function(x, s) {
  unlst(Map(substr, s, c(1L, x + 1L), c(x - 1L, nchar(s))))
}

greg <- function(...) {
  res <- lapply(gregexpr(...), `attributes<-`, NULL)
  res[vapply(res, identical, logical(1L), -1L)] <- list(integer())
  res
}

find_paren <- function(x) {

  sta <- greg("\\(", x)
  end <- greg("\\)", x)

  res <- Map(rep, 0L, nchar(x))
  res <- Map(`[<-`, res, sta, 1L)

  res <- Map(`[<-`, res, end, lapply(Map(`[`, res, end), `-`, 1L))
  res <- lapply(res, cumsum)

  res <- lapply(res, `==`, 0L)
  res <- Map(`[<-`, res, Map(c, sta, end), NA_integer_)

  res
}

expand_lst <- function(x) {
  apply(do.call(expand.grid, x), 1L, paste0, collapse = "")
}

seq_nna <- function(x) {
  nna <- !is.na(x)
  x[nna] <- seq_len(sum(nna))
  x
}

rep_paren <- function(x) {

  rln <- lapply(find_paren(x), rle)
  sqs <- lapply(lapply(rln, `[[`, "values"), seq_nna)
  rln <- Map(`[[<-`, rln, "values", sqs)
  rln <- lapply(rln , inverse.rle)

  res <- Map(split, strsplit(x, ""), rln)
  res <- lapply(res, lapply, paste0, collapse = "")

  res <- lapply(res, lapply, split_rgx)
  res <- lapply(res, expand_lst)

  unlst(res)
}

split_pipe <- function(x) {

  ind <- find_paren(x)

  spt <- greg("\\|", x)
  tdo <- lengths(spt) > 0L

  spt <- Map(`[`, spt, Map(`[`, ind, spt))
  res <- Map(split_ind, spt, x)

  res[tdo] <- lapply(res[tdo], rep_paren)

  unlst(res)
}

split_rgx <- function(rgx) unlst(split_pipe(rgx))

split_items <- function(x) {

  if (is_concept(x)) {
    return(new_concept(lapply(x, split_items)))
  }

  if (inherits(x, "sel_itm")) {
    return(rep_itm(x, "ids"))
  }

  if (inherits(x, "rgx_itm")) {
    return(rep_itm(x, "regex", split_rgx))
  }

  if (inherits(x, "itm")) {
    return(as_item(x))
  }

  if (inherits(x, "rec_cncpt")) {
    x$items <- as_concept(lapply(x$items, split_items))
  } else if (inherits(x, "cncpt")) {
    x$items <- do.call("c", lapply(x$items, split_items))
  }

  x
}

empty_items <- list()

report_empty <- function(dict, ...) {

  itm_load_report_empty <- quote({

    res <- returnValue()

    if (!nrow(res) && !inherits(x, "nul_itm")) {

      i <- 1

      while (!identical(.GlobalEnv, env <- parent.frame(i)) &&
             !inherits(cncpt <- get0("x", envir = env), "cncpt")) {
        i <- i + 1
      }

      if (inherits(x, "rgx_itm")) {
        id <- x$regex
      } else {
        id <- x$ids
      }

      add_empty <- list(name = cncpt$name, id = id, source = x$src,
                        table = x$table)
      empty_items <<- c(empty_items, list(add_empty))
    }

    res
  })

  trace(do_itm_load, exit = itm_load_report_empty, print = FALSE)
  on.exit(untrace(do_itm_load))

  load_concepts(split_items(dict), ...)

  empty_items
}

## ---- itm-check, cache = TRUE, eval = srcs_avail(demo)--------------------
demo <- c("mimic_demo", "eicu_demo")
concepts <- c("map", "lact", "bili", "gcs", "abx")

dict <- load_dictionary(demo, concepts)

empty_items <- report_empty(dict, merge = FALSE, verbose = FALSE)

## ---- itm-print, eval = length(get0("empty_items")) > 0L, echo = FALSE----
knitr::kable(
  data.table::rbindlist(empty_items),
  caption = paste(
    "For the given concepts, the listed item IDs do not return any data when",
    "the respective tables and data sources are queried."
  )
)

## ---- full-miss, echo = FALSE, eval = srcs_avail(demo) && !srcs_avail(srcs), results = "asis"----
#  demo_instead_full_msg(demo, srcs, "uom.html")

## ---- demo-srcs, eval = FALSE, echo = FALSE-------------------------------
#  srcs <- demo

## ---- itm-funs, eval = FALSE, echo = FALSE--------------------------------
#  count_meas <- function(x) {
#    x[!is.na(get(data_var(x))), list(count = .N), by = c(id_vars(x))]
#  }
#  
#  meas_day <- function(x, los) {
#    merge(x, los)[, count := count / los_icu]
#  }
#  
#  quants <- function(x) {
#    setNames(
#      as.list(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95))),
#      c("min", "lwr", "med", "upr", "max")
#    )
#  }
#  
#  meas_stats <- function(x, concept) {
#    x[, c(list(concept = concept, n_pat = .N), quants(count / los_icu)),
#      by = "source"]
#  }
#  

## ---- itm-counts, eval = FALSE, echo = FALSE------------------------------
#  
#  los <- load_concepts("los_icu", srcs, verbose = FALSE)
#  los <- los[los_icu > 0, ]
#  
#  concepts <- c("map", "lact", "crea", "bili", "plt")
#  
#  dat <- load_concepts(concepts, srcs, merge = FALSE, verbose = FALSE)
#  
#  counts <- lapply(dat, count_meas)
#  counts <- lapply(counts, merge, los)
#  counts <- Map(meas_stats, counts, names(counts))
#  counts <- do.call(rbind, counts)
#  counts <- merge(counts, los[, list(total_pat = .N), by = "source"],
#                  by = "source")
#  
#  head(counts)

## ---- itm-load, cache = TRUE, ref.label = c("itm-funs", if (srcs_avail(srcs)) "assign-srcs" else "demo-srcs", "itm-counts")----
count_meas <- function(x) {
  x[!is.na(get(data_var(x))), list(count = .N), by = c(id_vars(x))]
}

meas_day <- function(x, los) {
  merge(x, los)[, count := count / los_icu]
}

quants <- function(x) {
  setNames(
    as.list(quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95))),
    c("min", "lwr", "med", "upr", "max")
  )
}

meas_stats <- function(x, concept) {
  x[, c(list(concept = concept, n_pat = .N), quants(count / los_icu)),
    by = "source"]
}

srcs <- c("mimic", "eicu", "aumc", "hirid", "miiv")

los <- load_concepts("los_icu", srcs, verbose = FALSE)
los <- los[los_icu > 0, ]

concepts <- c("map", "lact", "crea", "bili", "plt")

dat <- load_concepts(concepts, srcs, merge = FALSE, verbose = FALSE)

counts <- lapply(dat, count_meas)
counts <- lapply(counts, merge, los)
counts <- Map(meas_stats, counts, names(counts))
counts <- do.call(rbind, counts)
counts <- merge(counts, los[, list(total_pat = .N), by = "source"],
                by = "source")

head(counts)

## ---- count-plot, echo = FALSE, fig.width = 6-----------------------------
boxplots <- ggplot(counts, aes(concept)) +
  geom_boxplot(
     aes(ymin = min, lower = lwr, middle = med, upper = upr, ymax = max,
         color = source),
     stat = "identity"
  ) +
  coord_flip() +
  theme_bw() +
  xlab("Concept name") + ylab("Measurement count per ICU day")

pat_perc <- ggplot(counts, aes(concept)) +
  geom_col(aes(y = n_pat / total_pat, fill = source), position = "dodge2") +
  coord_flip() +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab("Percentage of patients")

if (requireNamespace("cowplot", quietly = TRUE)) {
  cowplot::plot_grid(
    cowplot::plot_grid(
      boxplots + theme(legend.position = "none"),
      pat_perc + theme(legend.position = "none"),
      nrow = 1L, rel_widths = c(0.7, 0.3)
    ),
    cowplot::get_legend(boxplots + theme(legend.position = "bottom")),
    ncol = 1L, rel_heights = c(0.9, 0.1)
  )
} else {
  boxplots
  pat_perc
}

## ---- uom-hist, echo = FALSE, fig.width = 6-------------------------------
filter_quants <- function(x, lwr, upr) {

  do_filter <- function(x) {
    qq <- quantile(x, probs = c(lwr, upr), na.rm = TRUE)
    x[!is.na(x) & x >= qq[1L] & x <= qq[2L]]
  }

  x[, list(val = do_filter(get(data_var(x)))), by = "source"]
}

for (x in dat) {

  feat <- data_var(x)

  x <- filter_quants(x, lwr = 0.025, upr = 0.975)

  title <- x[, list(val = median(val)), by = "source"]
  title <- paste0(title$val, " (", title$source, ")", collapse = ", ")
  title <- paste0(feat, ": ", title)

  print(
    ggplot(x, aes(x = val, fill = source)) +
      geom_density(alpha = 0.5) +
      xlab(feat) + theme_bw() + ggtitle(title)
  )
}

