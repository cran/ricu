% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callback-itm.R
\name{add_concept}
\alias{add_concept}
\alias{add_weight}
\alias{calc_dur}
\alias{combine_callbacks}
\title{Internal item callback utilities}
\usage{
add_concept(x, env, concept, var_name = concept, aggregate = NULL)

add_weight(x, env, var_name = "weight")

calc_dur(x, val_var, min_var, max_var, grp_var = NULL)

combine_callbacks(...)
}
\arguments{
\item{x}{Object in loading}

\item{env}{Data source environment as available as \code{env} in callback
functions}

\item{concept}{String valued concept name that will be loaded from the
default dictionary}

\item{var_name}{String valued variable name}

\item{aggregate}{Forwarded to \code{\link[=load_concepts]{load_concepts()}}}

\item{val_var}{String valued column name corresponding to the value variable}

\item{min_var, max_var}{Column names denoting start and end times}

\item{grp_var}{Optional grouping variable (for example linking infusions)}

\item{...}{Functions which will be successively applied}
}
\value{
A copy of \code{x} with the requested concept merged in.
}
\description{
The utility function \code{add_concept()} is exported for convenience when adding
external datasets and integrating concepts that require other concepts.
While this could be solves by defining a \code{rec_concpt}, in some scenarios
this might not be ideal, as it might be only required that \code{itm}
implementations for certain data sources require additional information.
Examples for this include vasopressor rates which might rely on patient
weight, and blood cell counts when expressed as ratio. For performance
reasons, the pulled in concept is internally cached, as this might be used
unchanged many times, when loading several concepts that need to pull in
the given concept. Persistence of cache is session-level and therefore this
utility is intended to be used somewhat sparingly.
}
\keyword{internal}
