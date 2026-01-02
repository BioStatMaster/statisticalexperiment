# Run full n-grid simulation across all algorithms and plot results.

source("experiment.R")

methods_to_run <- c("DCA", "DCA_fast", "BDCA_fast")

result <- run_n_experiment(
  n_grid = c(200, 500, 1000, 2000),
  R_rep = 3,
  methods_to_run = methods_to_run,
  do_tune = TRUE,
  criterion = "MSE",
  verbose_each_iter = TRUE
)

plot_obj <- plot_metrics_vs_n(result$res_mean)
print(plot_obj)
