BDA400 Assignment 6 - Technical Analysis Portfolio Dashboard 

Student: Manpreet Kaur 
Course: BDA400 - Data Science Tools and Techniques 
Assignment: Assignment 6 - Technical Analysis Portfolio Dashboard 

Project Description 

This project is an interactive R Shiny portfolio dashboard that retrieves historical stock-market data from Yahoo Finance. The dashboard allows the user to select a stock symbol, date range, time frame, and chart type. It calculates Moving Averages, RSI, and MACD and uses a 20-period/50-period moving-average rule to generate Buy, Sell, or Hold signals. Buy and Sell signal changes are annotated on the stock-price visualization. 

Features 

● Yahoo Finance historical stock-data retrieval with quantmod 

● Daily, weekly, and monthly time frames 

● Line, area, and candlestick-style stock charts 

● 20-period and 50-period simple moving averages 

● RSI (14) 

● MACD (12, 26, 9) 

● Buy/Sell/Hold trading rule 

● Buy/Sell annotations 

● Latest signal summary 

Required R Packages 

shiny, ggplot2, quantmod, TTR, dplyr, and xts 

The R script checks for missing packages and installs them before loading the application. 

Running the Project 

1. Open Manpreet_Kaur_BDA400_Assignment_6.R in RStudio. 

2. Make sure you have an internet connection so Yahoo Finance data can be retrieved. 

3. Run the entire script or click Run App. 

4. Enter a valid Yahoo Finance stock symbol, choose a date range and visualization options, and review the technical indicators and trading signals. 

Trading Rule 

● Buy: SMA(20) is above SMA(50) 

● Sell: SMA(20) is below SMA(50) 

● Hold: insufficient data or the two averages are equal 

Repository Link 

manpreetjohal0004/Manpreet_Kaur_BDA400_Assignment_6 

 

 
