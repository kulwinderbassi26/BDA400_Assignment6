# BDA400 - Assignment 6: Technical Analysis Dashboard

**Student Name:** [Your Full Name]  
**Course:** BDA400 / Data Science Tools and Techniques  
**Assignment:** Assignment 6 - Technical Analysis using R, Visualization Phase  
**Submission Date:** Session 15  

---

## Project Overview
This repository contains an interactive R Shiny application for real-time stock visualization and algorithmic trading signal analysis. The dashboard fetches historical data directly from Yahoo Finance and allows dynamic exploration of moving averages, signal crossovers, and technical indicator overlays.

## Features
- **Data Source:** Fetches historical equity data using `quantmod`.
- **Customizable Controls:** Date range pickers, chart type toggle (Line/Area), and configurable short/long moving average windows.
- **Technical Indicators:** Moving Average Crossover strategy, RSI, and MACD indicators.
- **Automated Annotations:** Highlights explicit 'Buy' and 'Sell' crossover points directly on the chart canvas.

## Instructions to Run
1. Open RStudio.
2. Ensure required packages are installed (`shiny`, `ggplot2`, `quantmod`, `TTR`, `dplyr`).
3. Run `shiny::runApp("app.R")`.
