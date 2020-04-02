context("detection parameters")
test_that("changing the detection rate should change diagnosed cases", {

  param_vec <- load_parameters()

  # run a simulation with some positive detection rates

  det_table <- load_detection_table()

  test = run_param_vec(
    params = param_vec, params2=NULL, days_out1 = 30, 
    days_out2 = NULL, days_out3 = 30, model_type = run_basic, det_table = det_table)

  # run a simulation with detection rates = 0

  det_table$rdetecti <- 0
  det_table$rdetecta <- 0

  test2 = run_param_vec(
    params = param_vec, params2=NULL, days_out1 = 30, 
    days_out2 = NULL, days_out3 = 30, model_type = run_basic, det_table = det_table)

  # we should expect some differences between a simulation run with 
  # positive detection rates vs. detection rates of 0 
  expect_gt(max(test - test2), 0)
})
