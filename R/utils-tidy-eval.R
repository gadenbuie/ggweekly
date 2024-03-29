#' Tidy eval helpers
#'
#' @description
#'
#' * \code{\link[rlang]{sym}()} creates a symbol from a string and
#'   \code{\link[rlang]{syms}()} creates a list of symbols from a
#'   character vector.
#'
#' * \code{\link[rlang]{expr}()} and \code{\link[rlang]{quo}()} quote
#'   one expression. `quo()` wraps the quoted expression in a quosure.
#'
#'   The plural variants [rlang::exprs()] and
#'   \code{\link[rlang]{quos}()} return a list of quoted expressions or
#'   quosures.
#'
#' * \code{\link[rlang]{enexpr}()} and \code{\link[rlang]{enquo}()}
#'   capture the expression supplied as argument by the user of the
#'   current function (`enquo()` wraps this expression in a quosure).
#'
#'   \code{\link[rlang]{enexprs}()} and \code{\link[rlang]{enquos}()}
#'   capture multiple expressions supplied as arguments, including
#'   `...`.
#'
#' `exprs()` is not exported to avoid conflicts with `Biobase::exprs()`,
#' therefore one should always use `rlang::exprs()`.
#'
#' To learn more about tidy eval and how to use these tools, visit
#' <http://rlang.r-lib.org> and the [Metaprogramming
#' section](https://adv-r.hadley.nz/introduction-16.html) of [Advanced
#' R](https://adv-r.hadley.nz).
#'
#' @md
#' @name     tidyeval
#' @keywords internal
#' @importFrom rlang quo quos enquo enquos quo_name sym ensym syms
#'                   ensyms expr exprs enexpr enexprs .data :=
#' @aliases  quo quos enquo enquos quo_name
#'           sym ensym syms ensyms
#'           expr exprs enexpr enexprs
#'           .data :=
NULL
