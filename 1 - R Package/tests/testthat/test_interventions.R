context("run_int intervention simulator")
test_that("base case run as run_basic or run_int works the same", {

  # the motivation of this test is to ensure that an intervention 
  # with no difference from the base case should have no difference 
  # from the base case. 

  # this test is important because when interventions are simulated, 
  # an event is used in the differential equations solver (run_int).
  # 

  param_vec <- load_parameters()

  det_table <- load_detection_table()

  test = run_param_vec(
    params = param_vec, params2=NULL, days_out1 = 30, 
    days_out2 = NULL, model_type = run_basic, det_table = det_table)

  test_int = run_param_vec(params = param_vec, params2 = param_vec, 
    days_out1 = 10, days_out2 = 30, model_type = run_int, det_table = det_table)

  expect_lt(max(test - test_int), .01)
})
