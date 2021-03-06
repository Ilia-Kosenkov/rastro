% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obs.R
\name{new_obs}
\alias{new_obs}
\alias{na_obs}
\alias{format.rastro_obs}
\alias{get_obs}
\alias{get_obs.default}
\alias{get_obs.rastro_obs}
\alias{get_err}
\alias{get_err.default}
\alias{get_err.rastro_obs}
\alias{vec_ptype_abbr.rastro_obs}
\alias{vec_ptype_full.rastro_obs}
\alias{vec_ptype2.rastro_obs}
\alias{vec_ptype2.rastro_obs.default}
\alias{vec_ptype2.rastro_obs.rastro_obs}
\alias{vec_ptype2.rastro_obs.double}
\alias{vec_ptype2.rastro_obs.integer}
\alias{vec_ptype2.integer.rastro_obs}
\alias{vec_ptype2.double.rastro_obs}
\alias{vec_ptype2.rastro_obs.data.frame}
\alias{vec_ptype2.rastro_obs.tbl_df}
\alias{vec_ptype2.data.frame.rastro_obs}
\alias{vec_ptype2.tbl_df.rastro_obs}
\alias{is_obs}
\alias{vec_cast.rastro_obs}
\alias{vec_cast.rastro_obs.default}
\alias{vec_cast.rastro_obs.rastro_obs}
\alias{vec_cast.rastro_obs.double}
\alias{vec_cast.rastro_obs.integer}
\alias{vec_cast.data.frame.rastro_obs}
\alias{vec_cast.tbl_df.rastro_obs}
\alias{vec_cast.rastro_obs.data.frame}
\alias{vec_cast.rastro_obs.tbl_df}
\alias{as.data.frame.rastro_obs}
\alias{as_tibble.rastro_obs}
\alias{as_obs}
\alias{vec_restore.rastro_obs}
\alias{vec_proxy.rastro_obs}
\alias{vec_proxy_compare.rastro_obs}
\alias{\%==\%.rastro_obs}
\alias{\%==\%.rastro_obs.default}
\alias{\%==\%.rastro_obs.rastro_obs}
\alias{vec_arith.rastro_obs}
\alias{vec_arith.rastro_obs.default}
\alias{vec_arith.rastro_obs.MISSING}
\alias{vec_arith.rastro_obs.numeric}
\alias{vec_arith.numeric.rastro_obs}
\alias{vec_arith.rastro_obs.rastro_obs}
\alias{vec_math.rastro_obs}
\title{Observation}
\usage{
new_obs(
  obs,
  err = missing_arg(),
  n_err = missing_arg(),
  p_err = missing_arg(),
  item_frmt = NA_character_
)

na_obs(item_ptype)

\method{format}{rastro_obs}(
  x,
  format = "{obs} (- {n_err}; + {p_err})",
  format_eq = "{obs} ± {err}",
  format_each = NULL,
  na_string = "NA_rastro_obs",
  ...
)

get_obs(x, ...)

\method{get_obs}{default}(x, ...)

\method{get_obs}{rastro_obs}(x, ...)

get_err(x, ...)

\method{get_err}{default}(x, ...)

\method{get_err}{rastro_obs}(x, ...)

\method{vec_ptype_abbr}{rastro_obs}(x, ...)

\method{vec_ptype_full}{rastro_obs}(x, ...)

vec_ptype2.rastro_obs(x, y, ...)

\method{vec_ptype2.rastro_obs}{default}(x, y, ..., x_arg = "x", y_arg = "y")

\method{vec_ptype2.rastro_obs}{rastro_obs}(x, y, ...)

\method{vec_ptype2.rastro_obs}{double}(x, y, ...)

\method{vec_ptype2.rastro_obs}{integer}(x, y, ...)

\method{vec_ptype2.integer}{rastro_obs}(x, y, ...)

\method{vec_ptype2.double}{rastro_obs}(x, y, ...)

\method{vec_ptype2.rastro_obs}{data.frame}(x, y, ..., x_arg = "x", y_arg = "y")

\method{vec_ptype2.rastro_obs}{tbl_df}(x, y, ..., x_arg = "x", y_arg = "y")

\method{vec_ptype2.data.frame}{rastro_obs}(x, y, ..., x_arg = "x", y_arg = "y")

\method{vec_ptype2.tbl_df}{rastro_obs}(x, y, ..., x_arg = "x", y_arg = "y")

is_obs(x, item_ptype)

vec_cast.rastro_obs(x, to, ...)

\method{vec_cast.rastro_obs}{default}(x, to, ...)

\method{vec_cast.rastro_obs}{rastro_obs}(x, to, ...)

\method{vec_cast.rastro_obs}{double}(x, to, ...)

\method{vec_cast.rastro_obs}{double}(x, to, ...)

\method{vec_cast.rastro_obs}{integer}(x, to, ...)

\method{vec_cast.data.frame}{rastro_obs}(x, to, ...)

\method{vec_cast.tbl_df}{rastro_obs}(x, to, ...)

\method{vec_cast.rastro_obs}{data.frame}(x, to, ...)

\method{vec_cast.rastro_obs}{tbl_df}(x, to, ...)

\method{as.data.frame}{rastro_obs}(x, ..., two_sided = FALSE)

\method{as_tibble}{rastro_obs}(x, ..., two_sided = FALSE)

as_obs(x)

\method{vec_restore}{rastro_obs}(x, to, ...)

\method{vec_proxy}{rastro_obs}(x, ...)

\method{vec_proxy_compare}{rastro_obs}(x, ...)

`\%==\%.rastro_obs`(x, y)

\method{\%==\%.rastro_obs}{default}(x, y)

\method{\%==\%.rastro_obs}{rastro_obs}(x, y)

vec_arith.rastro_obs(op, x, y, ...)

\method{vec_arith.rastro_obs}{default}(op, x, y, ...)

\method{vec_arith.rastro_obs}{MISSING}(op, x, y, ...)

\method{vec_arith.rastro_obs}{numeric}(op, x, y, ...)

\method{vec_arith.numeric}{rastro_obs}(op, x, y, ...)

\method{vec_arith.rastro_obs}{rastro_obs}(op, x, y, ...)

vec_math.rastro_obs(.fn, .x, ...)
}
\arguments{
\item{obs}{Observations.}

\item{err, p_err, n_err}{Symmetrical (\code{err}) or a pair of positive (\code{p_err}) and
negative (\code{n_err}) errors.}

\item{item_frmt}{A desired format string to control the output.}

\item{item_ptype}{Ptype of the contained data.}

\item{x, y}{\code{vec_ptype2*} arguments.}

\item{format, na_string, format_eq, format_each}{\code{glue} flromat strings (support interpolation).}

\item{...}{Additional parameters.}

\item{x_arg, y_arg}{\code{vec_ptype2*} and \code{vec_cast*} error message variable names.}

\item{to}{\code{vec_cast} argument.}

\item{two_sided}{Controls if both errors should be present in the converted \code{data.frame}/\code{tibble}.}

\item{op}{Arithmetic functions/operators.}

\item{.fn}{Mathematical function.}

\item{.x}{\code{vec_arith*} argument.}
}
\description{
Observation
}
