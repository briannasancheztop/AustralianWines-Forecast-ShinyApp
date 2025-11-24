1. Explore Tab
View time-series plots for each wine varietal
Filter by varietal
Interactive Plotly visualizations
Summary statistics and data preview

2. Train & Diagnose Tab
Fits three forecasting models:
TSLM (Time Series Linear Model)
ETS (Exponential Smoothing)
ARIMA (Auto ARIMA)

Allows user input:
Training cutoff (date selector)
Model selection

Displays:
Residual diagnostics
Model accuracy tables
ACF/PACF plots

3. Forecast Tab
Forecast future wine sales
Choose model and forecast horizon
Optional confidence intervals
Interactive forecast plots

├── app.R                       # Main Shiny App
├── AustralianWines.csv         # dataset
├── AustralianWinesSA.qmd       # Quarto report used to build the app
├── manifest.json               # Deployment dependency file for Posit Connect
├── data/                       # Supporting files 
├── README.md                   # Project documentation
└── posi_clous_setup.R          # Posit Connect deploy script

The dataset includes:

Monthly wine sales for:
Fortified
Red
Rosé
Sparkling
Sweet white
Dry white

Years: 1980–1995

Units: Thousand liters
