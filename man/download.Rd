% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup-download.R
\name{download_src}
\alias{download_src}
\alias{download_src.src_cfg}
\alias{download_src.aumc_cfg}
\alias{download_src.character}
\title{Data download utilities}
\usage{
download_src(x, data_dir = src_data_dir(x), ...)

\method{download_src}{src_cfg}(x, data_dir = src_data_dir(x), tables = NULL, force = FALSE, ...)

\method{download_src}{aumc_cfg}(
  x,
  data_dir = src_data_dir(x),
  tables = NULL,
  force = FALSE,
  token = NULL,
  verbose = TRUE,
  ...
)

\method{download_src}{character}(
  x,
  data_dir = src_data_dir(x),
  tables = NULL,
  force = FALSE,
  user = NULL,
  pass = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{x}{Object specifying the source configuration}

\item{data_dir}{Destination directory where the downloaded data is written
to.}

\item{...}{Generic consistency}

\item{tables}{Character vector specifying the tables to download. If
\code{NULL}, all available tables are downloaded.}

\item{force}{Logical flag; if \code{TRUE}, existing data will be re-downloaded}

\item{token}{Download token for AmsterdamUMCdb (see 'Details')}

\item{verbose}{Logical flag indicating whether to print progress information}

\item{user, pass}{PhysioNet credentials; if \code{NULL} and environment
variables \code{RICU_PHYSIONET_USER}/\code{RICU_PHYSIONET_PASS} are not set, user
input is required}
}
\value{
Called for side effects and returns \code{NULL} invisibly.
}
\description{
Making a dataset available to \code{ricu} consists of 3 steps: downloading
(\code{\link[=download_src]{download_src()}}), importing (\code{\link[=import_src]{import_src()}}) and attaching
(\code{\link[=attach_src]{attach_src()}}). While downloading and importing are one-time procedures,
attaching of the dataset is repeated every time the package is loaded.
Briefly, downloading loads the raw dataset from the internet (most likely
in \code{.csv} format), importing consists of some preprocessing to make the
data available more efficiently (by converting it to \code{\link[fst:fst]{.fst}}
format) and attaching sets up the data for use by the package.
}
\details{
Downloads by \code{ricu} are focused data hosted by
\href{https://physionet.org}{PhysioNet} and tools are currently available for
downloading the datasets
\href{https://physionet.org/content/mimiciii/1.4/}{MIMIC-III},
\href{https://physionet.org/content/eicu-crd/2.0/}{eICU} and
\href{https://physionet.org/content/hirid/1.0/}{HiRID} (see \link{data}). While
credentials are required for downloading any of the three datasets, demo
dataset for both MIMIC-III and eICU are available without having to log in.
Even though access to full dataset is credentialed, the datasets are in
fact publicly available. For setting up an account, please refer to \href{https://physionet.org/register/}{the registration form}.

PhysioNet credentials can either be entered in an interactive session,
passed as function arguments \code{user}/\code{pass} or as environment
variables \code{RICU_PHYSIONET_USER}/\code{RICU_PHYSIONET_PASS}. For setting
environment variables on session startup, refer to \code{\link[base:Startup]{base::.First.sys()}} and
for setting environment variables in general, refer to \code{\link[base:Sys.setenv]{base::Sys.setenv()}}
If the openssl package is available, SHA256 hashes of downloaded files are
verified using \code{\link[openssl:hash]{openssl::sha256()}}.

Demo datasets
\href{https://physionet.org/content/mimiciii-demo/1.4/}{MIMIC-III demo} and
\href{https://physionet.org/content/eicu-crd-demo/2.0/}{eICU demo} can either be
installed as R packages directly by running

\if{html}{\out{<div class="sourceCode">}}\preformatted{install.packages(
  c("mimic.demo", "eicu.demo"),
  repos = "https://eth-mds.github.io/physionet-demo"
)
}\if{html}{\out{</div>}}

or downloaded and imported using \code{\link[=download_src]{download_src()}} and \code{\link[=import_src]{import_src()}}.
Furthermore, \code{ricu} specifies \code{mimic.demo} and \code{eicu.demo} as \code{Suggests}
dependencies therefore, passing \code{dependencies = TURE} when calling
\code{\link[=install.packages]{install.packages()}} for installing \code{ricu}, this will automatically install
the demo datasets as well.

While the included data downloaders are intended for data hosted by
PhysioNet, \code{download_src()} is an S3 generic function that can be extended
to new classes. Method dispatch is intended to occur on objects that
inherit from or can be coerced to \code{src_cfg}. For more information on data
source configuration, refer to \code{\link[=load_src_cfg]{load_src_cfg()}}.

As such, with the addition of the AmsterdamUMCdb dataset, which
unfortunately is not hosted on PhysioNet, A separate downloader for that
dataset is available as well. Currently this requires both availability of
the CRAN package \code{xml2}, as well as the command line utility 7zip.
Furthermore, data access has to be \href{https://amsterdammedicaldatascience.nl/#amsterdamumcdb}{requested } and for
non-interactive download the download token has to be made available as
environment variable \code{RICU_AUMC_TOKEN} or passed as \code{token} argument to
\code{download_src()}. The download token can be retrieved from the URL provided
when granted access as by extracting the string followed by \verb{token=}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{https://example.org/?s=download&token=0c27af59-72d1-0349-aa59-00000a8076d9
}\if{html}{\out{</div>}}

would translate to

\if{html}{\out{<div class="sourceCode r">}}\preformatted{Sys.setenv(RICU_AUMC_TOKEN = "0c27af59-72d1-0349-aa59-00000a8076d9")
}\if{html}{\out{</div>}}

If the dependencies outlined above are not fulfilled, download and archive
extraction can be carried out manually into the corresponding folder and
\code{\link[=import_src]{import_src()}} can be run.
}
\examples{
\dontrun{

dir <- tempdir()
list.files(dir)

download_datasource("mimic_demo", data_dir = dir)
list.files(dir)

unlink(dir, recursive = TRUE)

}

}
