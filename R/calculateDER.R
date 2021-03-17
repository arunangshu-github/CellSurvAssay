#' Calculates the Dose Enhancement Ratio
#'
#' It helps you calculate the Dose Enhancement Ratio without going through the data wrangling steps.
#'
#' @param data A data frame containing at least the following five columns: "cline", "Exp", "dose", "ncells", "ncolonies".
#' @param control Name of the cell-line/group that will act as control (numerator in the ratio).
#' @param treatment Name of the cell-line/group that will act as treatment (denominator in the ratio).
#' @param S The survival fraction value for which you want to calculate your DER.
#' @param method Method used for the fit. It's \code{"ml"} (maximum likelihood) by default. Can be \code{"ls"} (least squares) or \code{"franken"} (weighted least squares as described by Franken eta al.(2006)).
#' @param PEmethod Controls the value of the plating efficiencies. \code{"fit"} calculates fitted plating efficiencies as model parameters, \code{"fix"} uses fixed ones calculated from the observed zero dose data or from a column named \code{pe} in \code{data}.
#' @return The Dose Enhancement Ratio of Control:Treatment.
#' @examples
#' calculateDER(datatab, HN31, FADU, 0.25)
#' calculateDER(datatab, HN31, FADU, 0.25, method = "ls", PEmethod = "fix")
#' @export
calculateDER <- function(data, control, treatment, S, method = "ml", PEmethod = "fit") {




  quadraticFunction <- function(a, b, c) {
    if(delta(a, b, c) > 0) {
      x_1 = (-b + sqrt(delta(a, b, c)))/(2*a)
      x_2 = (-b - sqrt(delta(a, b, c)))/(2*a)
      if (x_1 >= 0 & x_2 >= 0) {
        z <- readline(prompt = paste("There are two positive roots: a)", x_1, "& b)", x_2, ". Which one do you want the DER for? (please enter a/b): "))
        if (z == "a") {
          result <- x_1
        }else if (z == "b") {
          result <- x_2
        }else {
          print("Invalid entry! Please try again.")
        }
      }else if (x_1 >= 0 & x_2 < 0) {
        result <- x_1
      }else if(x_2 >= 0 & x_1 < 0) {
        result <- x_2
      }else {
        result <- NA
      }
    }
    else if(delta(a,b,c) == 0){ # second case D=0
      result <- -b/(2*a)
    }
    else {result <- NA} # third case D<0
  }


  delta <- function(a,b,c) {
    b^2-4*a*c
  }




  control_data <- subset(data, cline == control)
  treatment_data <- subset(data, cline == treatment)
  invisible(utils::capture.output(fit_control <- CFAssay::cellsurvLQfit(control_data, method = method, PEmethod = PEmethod)))
  invisible(utils::capture.output(fit_treatment <- CFAssay::cellsurvLQfit(treatment_data, method = method, PEmethod = PEmethod)))
  alpha_control <- fit_control$coefficients[["dose"]] * (-1)
  beta_control <- fit_control$coefficients[["dose2"]] * (-1)
  alpha_treatment <- fit_treatment$coefficients[["dose"]] * (-1)
  beta_treatment <- fit_treatment$coefficients[["dose2"]] * (-1)
  D_control <- quadraticFunction(beta_control, alpha_control, log(S))
  D_treatment <- quadraticFunction(beta_treatment, alpha_treatment, log(S))
  DER <- D_control/D_treatment
  return(DER)
}
