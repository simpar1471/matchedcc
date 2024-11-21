# matchedcc: Stata-like matched case-control analysis
# Copyright (C) 2024 Simon Parker
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Example data for matched case-control analysis
#'
#' A subset of data from Jick *et al*. (1973) with data on a matched
#' case-control study on myocardial infarction and drinking 6+ cups of coffee
#' per day. Cases and controls were matched after excluding people who drank
#' 1 to 5 cups of coffee per day.
#'
#' @format ## `mccxmpl`
#' A data frame with 27 rows and 2 columns:
#' \describe{
#'   \item{`case`}{Integer variable of either `1` (exposed) or `0`
#'     (not exposed)}
#'   \item{`control`}{Integer variable of either `1` (exposed) or `0`
#'     (not exposed)}
#' }
#' @source In Stata 18 - run the commands:
#' ```
#' webuse mccxmpl, clear
#' expand pop
#' keep case control
#' ```
#' @references
#' Jick, H. *et al*. (1973). Coffee and myocardial infarction. **New England
#' Journal of Medicine** 289: 63–67. \doi{10.1056/NEJM197307122890203}.
"mccxmpl"