#' @title Insert breakpoint in a function
#' @description Interactively inserts a breakpoint on a function under development
#' @param fun_name name of the function
#' @return A traced function at the selected point
#' @author Waldir Leoncio
#' @export
#' @examples
insert_breakpoint <- function(fun_name) {
  # Primary point of entry
  fun_list <- as.list(body(fun_name))
  print(fun_list)
  point <- readline("Enter the point where you want to insert the breakpoint: ")

  # Secondary point of entry
  print(as.list(fun_list[[as.numeric(point)]]))
  subpoint <- readline("Enter the secondary point of entry (leave empty for none): ")

  # Processing points of entry
  at_point <- as.numeric(ifelse(subpoint == "", point, c(point, subpoint)))

  # Tracing function
  actual_function <- deparse(substitute(fun_name))
  trace(actual_function, browser, at = at_point)
  message("To remove the breakpoint, run `untrace(", actual_function, ")`.")
}
