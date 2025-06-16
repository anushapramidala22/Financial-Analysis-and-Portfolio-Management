# Set working directory - adjust this path to where your .RData files are located
# setwd("~/Desktop/Barclays Projects/Portfolio Theory")
# Load necessary libraries
library(moments) # For skewness and kurtosis calculations

# --- Load Data Files ---
# SP100.RData: Contains monthly stock returns and risk-free rates.
# FF3.RData: Contains Fama-French factors, including market excess return.

load("SP100.RData")
load("FF3.RData")


# --- Define Global Parameters ---
# Assigned stocks for the analysis
my_stocks <- c("MDT", "MMM", "MO", "MRK", "MS")

# Extract stock returns for the chosen stocks
stock_ret <- SP100[, my_stocks]

# Extract Risk-Free Rate (RF) from SP100 data
RF <- SP100$RF

# Extract Market Excess Return (MKT_ex) from FF3 data
MKT_ex <- FF3$MKT_ex # Used in CAPM calculations (Task 3)

# --- Task 1: Descriptive Statistics for Individual Stocks ---
print("--- Executing Task 1: Descriptive Statistics for Individual Stocks ---")

# Calculate mean returns for each stock
stock_mean_returns <- colMeans(stock_ret)

# Calculate standard deviation of returns for each stock
stock_std_dev <- apply(stock_ret, 2, sd)

# Calculate excess returns for Sharpe Ratio calculation
stock_excess_returns <- stock_ret - RF

# Calculate mean of excess returns
stock_mean_excess_returns <- colMeans(stock_excess_returns)

# Calculate standard deviation of excess returns
stock_std_dev_excess_returns <- apply(stock_excess_returns, 2, sd)

# Calculate Sharpe Ratios
stock_sharpe_ratios <- stock_mean_excess_returns / stock_std_dev_excess_returns

# Consolidate and print Task 1 results
task1_summary_stats <- data.frame(
  Mean_Return = stock_mean_returns,
  Standard_Deviation = stock_std_dev,
  Sharpe_Ratio = stock_sharpe_ratios
)
print("Summary Statistics for Individual Stocks (Task 1):")
print(round(task1_summary_stats, 6))


# --- Task 2: Plot Wealth Evolution for Individual Stocks ---
print("--- Executing Task 2: Plotting Wealth Evolution of Individual Stocks ---")

# Calculate cumulative returns, starting with an initial wealth of 1
# The wealth_evolution matrix will have an initial row of 1s
cumulative_stock_returns <- apply(1 + stock_ret, 2, cumprod)
wealth_evolution_stocks <- rbind(rep(1, ncol(stock_ret)), cumulative_stock_returns)

# Prepare date vector for plotting
stock_dates <- as.Date(SP100$date)
# Estimate period length to find a date for time 0
period_length_approx <- if (length(stock_dates) >= 2) diff(stock_dates[1:2]) else 30
date_time0_stocks <- stock_dates[1] - period_length_approx
plot_dates_stocks <- c(date_time0_stocks, stock_dates)

# Plotting
if (length(plot_dates_stocks) == nrow(wealth_evolution_stocks)) {
  stock_plot_colors <- c("black", "red", "blue", "green4", "purple") # Colors for 5 stocks
  y_range_stocks <- range(wealth_evolution_stocks, na.rm = TRUE)
  
  plot(wealth_evolution_stocks[, 1] ~ plot_dates_stocks, type = "l", col = stock_plot_colors[1],
       ylim = y_range_stocks, xlab = "Time", ylab = "Value of $1 Investment",
       main = "Wealth Evolution of Individual Stocks", lwd = 2, xaxt = "n")
  axis.Date(1, at = seq(min(plot_dates_stocks, na.rm=T), max(plot_dates_stocks, na.rm=T), by="3 years"), format = "%Y")
  
  
  if (ncol(wealth_evolution_stocks) > 1) {
    for (i in 2:ncol(wealth_evolution_stocks)) {
      lines(wealth_evolution_stocks[, i] ~ plot_dates_stocks, col = stock_plot_colors[i], lwd = 2)
    }
  }
  legend("topleft", legend = colnames(stock_ret), col = stock_plot_colors, lwd = 2, bty = "n", cex = 0.8)
} else {
  print("Error: Length mismatch for Task 2 plot. Skipped.")
}

# --- Task 3: CAPM Expected Returns and Comparison ---
print("--- Executing Task 3: CAPM Expected Returns & Comparison ---")

# Data for CAPM: stock_excess_returns, RF, MKT_ex (defined globally)
total_observations <- nrow(stock_excess_returns)
split_point_h <- total_observations / 2

# First Half Data
stock_exret_h1 <- stock_excess_returns[1:split_point_h, ]
mkt_ex_h1 <- MKT_ex[1:split_point_h]
rf_h1_mean <- mean(RF[1:split_point_h])

# Second Half Data
stock_exret_h2 <- stock_excess_returns[(split_point_h + 1):total_observations, ]
mkt_ex_h2 <- MKT_ex[(split_point_h + 1):total_observations]
rf_h2_mean <- mean(RF[(split_point_h + 1):total_observations])

# Function to calculate CAPM betas and expected returns
calculate_capm_estimates <- function(stock_ex_returns, market_ex_returns, mean_rf) {
  num_stocks_capm <- ncol(stock_ex_returns)
  capm_betas <- numeric(num_stocks_capm)
  capm_expected_returns <- numeric(num_stocks_capm)
  names(capm_betas) <- colnames(stock_ex_returns)
  names(capm_expected_returns) <- colnames(stock_ex_returns)
  
  mean_market_ex_returns <- mean(market_ex_returns, na.rm = TRUE)
  
  for (i in 1:num_stocks_capm) {
    model_fit <- lm(stock_ex_returns[, i] ~ market_ex_returns)
    if (!is.null(model_fit) && length(model_fit$coefficients) == 2 && !any(is.na(model_fit$coefficients))) {
      capm_betas[i] <- model_fit$coefficients[2] # Beta is the slope coefficient
      capm_expected_returns[i] <- mean_rf + capm_betas[i] * mean_market_ex_returns
    } else {
      capm_betas[i] <- NA
      capm_expected_returns[i] <- NA
    }
  }
  return(list(betas = capm_betas, expected_returns = capm_expected_returns))
}

# Calculate for H1
capm_h1_results <- calculate_capm_estimates(stock_exret_h1, mkt_ex_h1, rf_h1_mean)
E_ret_capm_h1 <- capm_h1_results$expected_returns
print("CAPM Expected Returns (First Half - H1):")
print(round(E_ret_capm_h1, 6))

# Calculate for H2
capm_h2_results <- calculate_capm_estimates(stock_exret_h2, mkt_ex_h2, rf_h2_mean)
E_ret_capm_h2 <- capm_h2_results$expected_returns
print("CAPM Expected Returns (Second Half - H2):")
print(round(E_ret_capm_h2, 6))

# Plot H1 vs H2 CAPM Estimates
plot_range_capm <- range(c(E_ret_capm_h1, E_ret_capm_h2), na.rm = TRUE)
plot_lim_capm <- if (all(is.na(plot_range_capm))) c(0,0.01) else plot_range_capm + c(-1, 1) * diff(plot_range_capm) * 0.15

plot(x = E_ret_capm_h1, y = E_ret_capm_h2,
     xlab = "CAPM Expected Return (First Half)", ylab = "CAPM Expected Return (Second Half)",
     main = "Comparison of CAPM Estimates: H1 vs H2",
     pch = 19, col = "blue", xlim = plot_lim_capm, ylim = plot_lim_capm)
text(x = E_ret_capm_h1, y = E_ret_capm_h2, labels = names(E_ret_capm_h1), pos = 4, cex = 0.7)
abline(a = 0, b = 1, col = "red", lty = 2) # 45-degree reference line

# --- Task 4: Portfolio Construction with Expanding Window ---
print("--- Executing Task 4: Portfolio Weight Calculation (Expanding Window) ---")

initial_window_size <- 60
N_stocks_portfolio <- ncol(stock_excess_returns) # Number of stocks for portfolio
total_obs_portfolio <- nrow(stock_excess_returns)
V1_portfolio <- rep(1, N_stocks_portfolio) 

num_rebalances <- floor((total_obs_portfolio - initial_window_size) / 12)

W_GMV_list <- list()
W_MV_list <- list()
oos_period_start_indices <- numeric(num_rebalances)
estimation_end_indices <- numeric(num_rebalances)
MV_target_returns_list <- numeric(num_rebalances)

print(paste("Number of annual rebalances to perform:", num_rebalances))

for (k in 1:num_rebalances) {
  current_estimation_end_period <- if (k == 1) initial_window_size else initial_window_size + (k - 1) * 12
  estimation_end_indices[k] <- current_estimation_end_period
  oos_period_start_indices[k] <- current_estimation_end_period + 1
  
  # print(paste("Rebalance", k, "- Estimating with data up to month:", current_estimation_end_period))
  
  current_estimation_exret <- stock_excess_returns[1:current_estimation_end_period, , drop = FALSE]
  
  if(nrow(current_estimation_exret) < N_stocks_portfolio) {
    W_GMV_list[[k]] <- rep(NA, N_stocks_portfolio); W_MV_list[[k]] <- rep(NA, N_stocks_portfolio); MV_target_returns_list[k] <- NA; next
  }
  
  Sigma_current_est <- cov(current_estimation_exret, use = "pairwise.complete.obs") # Handle potential NAs in exret
  Mu_current_est <- colMeans(current_estimation_exret, na.rm = TRUE)
  
  # GMV Portfolio
  w_gmv_k <- rep(NA, N_stocks_portfolio)
  Sigma_inv_k <- tryCatch(solve(Sigma_current_est), error = function(e) NULL)
  
  if (!is.null(Sigma_inv_k)) {
    denominator_gmv_k <- as.numeric(t(V1_portfolio) %*% Sigma_inv_k %*% V1_portfolio)
    if (!is.na(denominator_gmv_k) && denominator_gmv_k != 0) {
      w_gmv_k <- (Sigma_inv_k %*% V1_portfolio) / denominator_gmv_k
    }
  }
  W_GMV_list[[k]] <- as.vector(w_gmv_k)
  
  # MV Portfolio
  target_ret_mv_k <- mean(Mu_current_est, na.rm = TRUE)
  MV_target_returns_list[k] <- target_ret_mv_k
  w_mv_k <- rep(NA, N_stocks_portfolio)
  
  if (!is.null(Sigma_inv_k) && !is.na(denominator_gmv_k) && denominator_gmv_k != 0) {
    A_mv <- as.numeric(t(Mu_current_est) %*% Sigma_inv_k %*% Mu_current_est)
    B_mv <- as.numeric(t(V1_portfolio) %*% Sigma_inv_k %*% Mu_current_est)
    C_mv <- denominator_gmv_k 
    
    delta_mv_den <- A_mv * C_mv - B_mv^2
    if (abs(delta_mv_den) > 1e-9) {
      lambda_num <- (C_mv * target_ret_mv_k - B_mv)
      gamma_num  <- (A_mv - B_mv * target_ret_mv_k)
      w_mv_k <- Sigma_inv_k %*% ((lambda_num / delta_mv_den) * Mu_current_est + (gamma_num / delta_mv_den) * V1_portfolio)
    } else if (!any(is.na(w_gmv_k))) { # Fallback to GMV if valid
      w_mv_k <- w_gmv_k
    }
  }
  W_MV_list[[k]] <- as.vector(w_mv_k)
}
print("Portfolio weight calculations (Task 4) complete.")

# --- Task 5: Out-of-Sample Performance Evaluation ---
print("--- Executing Task 5: Out-of-Sample Performance ---")

oos_exret_gmv_gross_series <- numeric(0)
oos_exret_mv_gross_series <- numeric(0)

for (k in 1:num_rebalances) {
  w_gmv_oos_k <- W_GMV_list[[k]]
  w_mv_oos_k <- W_MV_list[[k]]
  
  start_idx_oos <- oos_period_start_indices[k]
  end_idx_oos <- if (k < num_rebalances) oos_period_start_indices[k+1] - 1 else total_obs_portfolio
  
  if (start_idx_oos > total_obs_portfolio || start_idx_oos > end_idx_oos) next
  
  current_block_exret_oos <- as.matrix(stock_excess_returns[start_idx_oos:end_idx_oos, , drop = FALSE])
  num_months_in_block <- nrow(current_block_exret_oos)
  
  oos_exret_gmv_gross_series <- c(oos_exret_gmv_gross_series, 
                                  if (!is.null(w_gmv_oos_k) && !all(is.na(w_gmv_oos_k))) as.vector(current_block_exret_oos %*% w_gmv_oos_k) else rep(NA, num_months_in_block))
  oos_exret_mv_gross_series <- c(oos_exret_mv_gross_series, 
                                 if (!is.null(w_mv_oos_k) && !all(is.na(w_mv_oos_k))) as.vector(current_block_exret_oos %*% w_mv_oos_k) else rep(NA, num_months_in_block))
}

# Align with the full out-of-sample RF series
oos_rf_series_aligned <- RF[(initial_window_size + 1):total_obs_portfolio]
expected_oos_periods <- total_obs_portfolio - initial_window_size

pad_truncate_vec <- function(vec, len) { # Helper function
  if (length(vec) < len) c(vec, rep(NA, len - length(vec))) else if (length(vec) > len) vec[1:len] else vec
}
oos_exret_gmv_gross_series <- pad_truncate_vec(oos_exret_gmv_gross_series, expected_oos_periods)
oos_exret_mv_gross_series <- pad_truncate_vec(oos_exret_mv_gross_series, expected_oos_periods)

R_gmv_gross_oos <- oos_exret_gmv_gross_series + oos_rf_series_aligned
R_mv_gross_oos <- oos_exret_mv_gross_series + oos_rf_series_aligned

# Performance metrics calculation function
calc_perf_metrics <- function(total_rets, excess_rets, name) {
  clean_total_rets <- na.omit(total_rets)
  clean_excess_rets <- na.omit(excess_rets)
  if (length(clean_total_rets) < 2 || length(clean_excess_rets) < 2) {
    return(data.frame(Portfolio=name, Mean=NA, StdDev=NA, Skewness=NA, Kurtosis=NA, Sharpe_Ratio=NA))
  }
  data.frame(
    Portfolio = name,
    Mean = mean(clean_total_rets), StdDev = sd(clean_total_rets),
    Skewness = skewness(clean_total_rets), Kurtosis = kurtosis(clean_total_rets),
    Sharpe_Ratio = mean(clean_excess_rets) / sd(clean_excess_rets)
  )
}

metrics_gmv_gross_table <- calc_perf_metrics(R_gmv_gross_oos, oos_exret_gmv_gross_series, "GMV Gross")
metrics_mv_gross_table <- calc_perf_metrics(R_mv_gross_oos, oos_exret_mv_gross_series, "MV Gross")

# Transaction Costs
tc_rate_assumed <- 0.001 # 0.1% one-way
oos_exret_gmv_net_series <- oos_exret_gmv_gross_series
oos_exret_mv_net_series <- oos_exret_mv_gross_series

w_gmv_prev_tc <- rep(0, N_stocks_portfolio)
w_mv_prev_tc <- rep(0, N_stocks_portfolio)

for (k in 1:num_rebalances) {
  idx_oos_tc <- oos_period_start_indices[k] - initial_window_size
  
  if (!is.null(W_GMV_list[[k]]) && !all(is.na(W_GMV_list[[k]]))) {
    turnover_gmv_tc <- sum(abs(W_GMV_list[[k]] - w_gmv_prev_tc))
    cost_gmv_tc <- turnover_gmv_tc * tc_rate_assumed
    if (idx_oos_tc >= 1 && idx_oos_tc <= length(oos_exret_gmv_net_series) && !is.na(oos_exret_gmv_net_series[idx_oos_tc])) {
      oos_exret_gmv_net_series[idx_oos_tc] <- oos_exret_gmv_net_series[idx_oos_tc] - cost_gmv_tc
    }
    w_gmv_prev_tc <- W_GMV_list[[k]]
  }
  
  if (!is.null(W_MV_list[[k]]) && !all(is.na(W_MV_list[[k]]))) {
    turnover_mv_tc <- sum(abs(W_MV_list[[k]] - w_mv_prev_tc))
    cost_mv_tc <- turnover_mv_tc * tc_rate_assumed
    if (idx_oos_tc >= 1 && idx_oos_tc <= length(oos_exret_mv_net_series) && !is.na(oos_exret_mv_net_series[idx_oos_tc])) {
      oos_exret_mv_net_series[idx_oos_tc] <- oos_exret_mv_net_series[idx_oos_tc] - cost_mv_tc
    }
    w_mv_prev_tc <- W_MV_list[[k]]
  }
}

R_gmv_net_oos <- oos_exret_gmv_net_series + oos_rf_series_aligned
R_mv_net_oos <- oos_exret_mv_net_series + oos_rf_series_aligned

metrics_gmv_net_table <- calc_perf_metrics(R_gmv_net_oos, oos_exret_gmv_net_series, "GMV Net")
metrics_mv_net_table <- calc_perf_metrics(R_mv_net_oos, oos_exret_mv_net_series, "MV Net")

all_performance_metrics <- rbind(metrics_gmv_gross_table, metrics_mv_gross_table, metrics_gmv_net_table, metrics_mv_net_table)
print("Consolidated Out-of-Sample Performance Metrics (Task 5):")
# To round only the numeric columns for printing:
all_performance_metrics_printable <- all_performance_metrics
# List the names of the columns you want to round
numeric_metric_cols <- c("Mean", "StdDev", "Skewness", "Kurtosis", "Sharpe_Ratio")
all_performance_metrics_printable[numeric_metric_cols] <- round(all_performance_metrics_printable[numeric_metric_cols], 6)
print(all_performance_metrics_printable)
# --- Task 6: Plot Portfolio Wealth Evolution ---
print("--- Executing Task 6: Plotting Portfolio Wealth Evolution ---")

oos_plot_dates <- SP100$date[(initial_window_size + 1):total_obs_portfolio]
date_time0_port_plot <- SP100$date[initial_window_size]
final_plot_dates <- c(date_time0_port_plot, oos_plot_dates)

calc_cumulative_wealth <- function(ret_series) { if(all(is.na(ret_series))) rep(NA, length(ret_series)+1) else c(1, cumprod(1 + ret_series)) }
Wealth_gmv_gross <- calc_cumulative_wealth(R_gmv_gross_oos)
Wealth_mv_gross  <- calc_cumulative_wealth(R_mv_gross_oos)
Wealth_gmv_net   <- calc_cumulative_wealth(R_gmv_net_oos)
Wealth_mv_net    <- calc_cumulative_wealth(R_mv_net_oos)

# Plotting parameters
col_gmv_plot <- "red"; col_mv_plot <- "blue"
lty_gross_plot <- "dashed"; lty_net_plot <- "solid"
y_range_wealth <- range(c(Wealth_gmv_gross, Wealth_mv_gross, Wealth_gmv_net, Wealth_mv_net), na.rm = TRUE)
y_buffer_wealth <- if(all(is.na(y_range_wealth))) 0 else (y_range_wealth[2] - y_range_wealth[1]) * 0.1
y_limits_plot <- if(all(is.na(y_range_wealth))) c(0.5,1.5) else c(y_range_wealth[1] - y_buffer_wealth, y_range_wealth[2] + y_buffer_wealth)


plot(final_plot_dates, Wealth_gmv_gross, type = "l", col = col_gmv_plot, lty = lty_gross_plot, lwd = 2,
     ylim = y_limits_plot, xlab = "Time", ylab = "Wealth (Value of $1 Investment)",
     main = "Portfolio Wealth Evolution (Out-of-Sample)", xaxt = "n")
axis.Date(1, at = seq(min(final_plot_dates, na.rm=T), max(final_plot_dates, na.rm=T), by = "2 years"), format = "%Y-%m", cex.axis = 0.7)

lines(final_plot_dates, Wealth_gmv_net, col = col_gmv_plot, lty = lty_net_plot, lwd = 2)
lines(final_plot_dates, Wealth_mv_gross, col = col_mv_plot, lty = lty_gross_plot, lwd = 2)
lines(final_plot_dates, Wealth_mv_net, col = col_mv_plot, lty = lty_net_plot, lwd = 2)

legend("topleft", cex = 0.7, bty = "n", lwd = 2,
       legend = c("GMV Gross TC", "GMV Net TC", "MV Gross TC", "MV Net TC"),
       col = c(col_gmv_plot, col_gmv_plot, col_mv_plot, col_mv_plot),
       lty = c(lty_gross_plot, lty_net_plot, lty_gross_plot, lty_net_plot))

print("--- R Script Execution Complete ---")
