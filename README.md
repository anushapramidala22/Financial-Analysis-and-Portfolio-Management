# Financial Analysis and Portfolio Management

This repository showcases a collection of projects demonstrating skills in financial analysis, corporate valuation, econometric modeling, and portfolio management. The projects utilize tools such as R and Excel to analyze financial data, build predictive models, and apply investment theories.

---

## Projects

### 1. Tesla, Inc. Corporate Valuation
*A comprehensive financial modeling and valuation project for Tesla, Inc. (TSLA), performed as part of the Corporate Finance course at Masaryk University.*

> This project presents a complete financial valuation of Tesla, Inc., based on historical data from 2019-2023. Using Excel, a detailed financial model was constructed to forecast future performance and derive the company's intrinsic value. The final output is a detailed report that outlines the valuation methodology and provides a final investment recommendation.

**Key Methodologies & Features:**
- **Financial Statement Analysis:** Historical analysis of the Income Statement, Balance Sheet, and Cash Flow Statement.
- **Ratio Analysis:** Horizontal and vertical analysis to identify trends and assess financial health.
- **Corporate Lifecycle Assessment:** Evaluating Tesla's strategic position and growth stage.
- **Cost of Capital Calculation:** Detailed calculation of the Weighted Average Cost of Capital (WACC) using the CAPM model for the cost of equity.
- **Discounted Cash Flow (DCF) Valuation:** Building a multi-year forecast to determine the Free Cash Flow to the Firm (FCFF) and calculate the company's terminal value.

**Tools Used:**
- `Microsoft Excel` for all financial modeling, forecasting, and calculations.

---

### 2. Econometric Analysis of European Stock Markets
*An applied econometrics project analyzing the dynamic relationships between major European stock indices.*

> This project applies time series analysis to investigate the interdependencies between four major European stock indices: the DAX (Germany), SMI (Switzerland), CAC (France), and FTSE (UK). The analysis involves testing for stationarity, building appropriate models, and examining the predictive relationships between the markets. The project culminates in forecasting future index values.

**Key Steps & Techniques:**
- **Data Preparation:** Adjusting the time series data for seasonality.
- **Stationarity Testing:** Using the Augmented Dickey-Fuller (ADF) test to check if the series are stationary.
- **Model Estimation:** Building a Vector Autoregression (VAR) model to capture the linear interdependencies among the multiple time series.
- **Causality Testing:** Applying the Granger Causality test to determine if past values of one index are useful in predicting future values of another.
- **Forecasting:** Generating a 25-month forecast for each index based on the final VAR model.

**Tools Used:**
- `R`: For all statistical analysis, modeling, and visualization.
- **Key R Packages:** `vars`, `tseries`, `forecast`, `zoo`.

---

### 3. Portfolio Optimization and Performance Analysis
*A practical application of modern portfolio theory to construct and evaluate investment portfolios from a selection of S&P 100 stocks.*

> This analysis focuses on the practical application of portfolio theory using five stocks from the SP100 index (MDT, MMM, MO, MRK, MS). The project covers the entire workflow from analyzing individual assets to constructing, testing, and evaluating different portfolio strategies. The impact of real-world factors like transaction costs is also assessed.

**Core Concepts & Analysis:**
- **Individual Asset Analysis:** Calculating descriptive statistics, including mean, standard deviation, and Sharpe Ratio for each stock.
- **CAPM Estimation:** Using the Capital Asset Pricing Model to estimate expected returns for the assets.
- **Portfolio Construction:** Building two distinct portfolios using an expanding window approach:
    1.  **Global Minimum Variance (GMV) Portfolio:** Aims to minimize portfolio risk.
    2.  **Mean-Variance (MV) Portfolio:** Aims to achieve a target return.
- **Out-of-Sample Evaluation:** Assessing the performance of the portfolios on data not used in their construction.
- **Transaction Cost Impact:** Analyzing how transaction costs affect the net returns and overall wealth evolution of the strategies.

**Tools Used:**
- `R`: For all quantitative analysis, portfolio optimization, and performance measurement.
