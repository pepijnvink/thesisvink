#' Analyze data simulated with the `sim_scm` function
#'
#'  Data are analyzed with the RI-CLPM and the DPM (with and without free loadings).
#'
#' @param data An object with simulated data of type {.cls ThesisSimData} to perform analyses on.
#'
#' @return A list with the original data and results of the analyses.
#' @examples
#' \dontrun{
#' analyze_sim(data)
#' }
#' @export
analyze_sim <- function(data){
  if(!inherits(data, "ThesisSimData")){
    cli::cli_abort(c("{.var data} must be of class {.cls ThesisSimData}, created with the `sim_scm` function.",
                     "x" = "You've supplied an object of class {.cls {class(data)}}."))
  }
  data_short <- purrr::map(data$datasets, ~dplyr::select(., x1:y5))
  progressr::with_progress({
    progressr::handlers("cli")
    p1 <- progressr::progressor(along = data_short)
    res_riclpm <- furrr::future_map(data_short, function(x){
      p1(message = "RICLPM")
      lavaan::lavaan(riclpm, data = x)}, .options = furrr::furrr_options(seed = TRUE))
    p2 <- progressr::progressor(along = data_short)
    res_dpm <- furrr::future_map(data_short, function(x){
      p2(message = "DPM")
      lavaan::lavaan(dpm, data = x)}, .options = furrr::furrr_options(seed = TRUE))
    p3 <- progressr::progressor(along = data_short)
    res_riclpm_free <- furrr::future_map(data_short, function(x){
      p3(message = "RICLPM (free loadings)")
      lavaan::lavaan(riclpm_free, data = x)}, .options = furrr::furrr_options(seed = TRUE))
    p4 <- progressr::progressor(along = data_short)
    res_dpm_free <-  furrr::future_map(data_short, function(x){
      p4(message = "DPM (free loadings)")
      lavaan::lavaan(dpm_free, data = x)}, .options = furrr::furrr_options(seed = TRUE))
  results <- list(RICLPM = res_riclpm,
                  DPM = res_dpm,
                  `RICLPM (free loadings)` = res_riclpm_free,
                  `DPM (free loadings)` = res_dpm_free)})
  output <- append(data, list(results = results))
  return(output)
}
