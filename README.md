# Australian Wine Sales Forecasting – Interactive Shiny App

An interactive R Shiny dashboard that explores 15 years of Australian wine sales and compares three forecasting models side by side, so a user can see which approach works best for each wine varietal.

**Live app:** [Open on Posit Cloud](PASTE_YOUR_POSIT_CLOUD_LINK_HERE)

![App screenshot](images/forecast_tab.png)
<!-- Save a screenshot of the Forecast tab to images/forecast_tab.png -->

---

## Problem

Wine producers and distributors plan production, inventory, and staffing months in advance. Sales are strongly seasonal and differ a lot by varietal, so a single model rarely fits every product. This app lets a user test, diagnose, and compare models for each varietal before trusting a forecast.

## Dataset

- **Source:** Australian Wines monthly sales dataset (`AustralianWines.csv`, included in this repo)
- **Coverage:** Monthly, 1980–1995
- **Units:** Thousands of liters
- **Varietals:** Fortified, Red, Rosé, Sparkling, Sweet White, Dry White

## App Features

### 1. Explore
- Filter by varietal and date range
- Interactive Plotly time-series plot
- STL decomposition (trend, seasonality, remainder)
- Summary statistics (mean, median, standard deviation, min, max, missing months)
- Searchable data preview table

### 2. Train & Diagnose
- Fits three forecasting models:
  - **TSLM** – linear model with trend and seasonal terms
  - **ETS** – exponential smoothing (automatic selection)
  - **ARIMA** – automatic ARIMA selection
- **User inputs:** varietal, training cutoff date, and the model to diagnose
- **Outputs:**
  - Model specification
  - Training and validation accuracy tables for all three models (RMSE, MAE, MAPE, MASE, and more)
  - Residual diagnostics (residual plot, residual ACF, histogram)
  - Ljung-Box test for leftover autocorrelation in the residuals
  - ACF and PACF plots of the training series

### 3. Forecast
- Choose which models to display and the forecast horizon (6–60 months)
- Optional 80% and 95% prediction intervals
- Interactive Plotly forecast chart

## Methods

1. **Exploration:** visualize each series and use STL decomposition to separate trend, seasonality, and noise.
2. **Train/validation split:** months up to the chosen cutoff train the models, and the months after it are held out to measure forecast accuracy.
3. **Diagnostics:** check whether each model's residuals look like white noise (residual ACF plus the Ljung-Box test). If they do, the model has captured the pattern in the data.
4. **Forecasting:** refit the models on the full series and forecast forward with prediction intervals.

## Key Results

- **Seasonality dominates** most varietals, so seasonal models clearly outperform simple trend models.
- **ETS performed best** for Dry White and Fortified wines.
- **Rosé is the most volatile** series; it needs shorter forecast horizons and has wider prediction intervals, so its forecasts should be used with more caution.

<!-- Optional: add a small table of validation RMSE by varietal and model here -->

## Technologies

R · Shiny · bslib · tsibble · fable · feasts · fabletools · dplyr · tidyr · readr · ggplot2 · Plotly · DT · Quarto · Posit Cloud

## Setup & How to Run

**Requirements:** R 4.3+ and RStudio (recommended)

1. Clone the repository:
   ```bash
   git clone https://github.com/briannasancheztop/AustralianWines-Forecast-ShinyApp.git
   ```
2. Open `project.Rproj` in RStudio.
3. Install the exact package versions:
   ```r
   install.packages("renv")
   renv::restore()
   ```
   If `renv.lock` is not available, install the packages directly:
   ```r
   install.packages(c("shiny", "bslib", "readr", "dplyr", "tidyr", "ggplot2",
                      "tsibble", "feasts", "fable", "fabletools", "plotly", "DT"))
   ```
4. Launch the app:
   ```r
   shiny::runApp()
   ```

## Project Structure

```
├── app.R                    # Shiny app (UI + server)
├── AustralianWines.csv      # Dataset
├── AustralianWinesSA.qmd    # Quarto analysis report used to design the app
├── manifest.json            # Posit Cloud / Connect deployment manifest
├── renv.lock                # Pinned R package versions
├── project.Rproj            # RStudio project file
├── images/                  # README screenshots
├── .gitignore
└── README.md
```

## Limitations

- The data ends in 1995, so the app demonstrates forecasting methods rather than predicting current sales.
- Models are fit separately for each varietal; relationships between varietals are not modeled.
- ETS and ARIMA use automatic model selection; manually tuned models could perform better on some series.
- The same Ljung-Box lag (24) is used for every model.

## Next Steps

- Add a summary table of the best model per varietal based on validation accuracy.
- Add time-series cross-validation for more reliable model comparison.
- Let users download forecasts as a CSV.

## Author

**Brianna Sanchez** – M.S. Applied Data Science, University of San Diego
[LinkedIn](https://www.linkedin.com/in/brianna-sanchez-79523123a/)
