A line I wrote on my local computer 

# Airbnb Analysis and Prediction Website Application

## Authors

Qiran Hu ([qiranhu2\@illinois.edu](mailto:qiranhu2@illinois.edu))\
Zoey Yao ([wanjing4\@illinois.edu](mailto:wanjing4@illinois.edu))

## Project Proposal

This project aims to develop a Shiny website application that integrates with the Yelp Fusion Market API to analyze restaurant and market data, generate interactive visualizations, and develop predictive models. The application will provide valuable insights for restaurant owners, customers, and analysts by processing Yelp Fusion data to understand market trends, optimize pricing strategies, and predict future demand.

## Data

Through the Yelp Fusion Market API, we can obtain the most updated data from the website, ensuring our application provides accurate and real-time information for all audiences.

## Key Dimensions

-   Popularity/ratings - popularity of the restaurant

-   Cuisine categories and other features of the restaurant

## Research Question

The project will also answer the questions below

1.  What are the commonalities of the restaurants that have high ratings? (If someone wants to start a new restaurant, he can refer to these traits.)

2.  Is there a rating difference between each cuisine?

3.  If we need to register a newly opened restaurant, with all other secondary attributes/features, what might be its rating?

4.  If we need to register a newly opened restaurant, with all other secondary attributes/features, what might be its rating?

## Methodology

### Data Preprocessing

-   Data will be scraped from the web API using SQL.
-   PCA and correlation matrices will be used to reduce the high dimensionality of the dataset.
-   Continuous variables will be discretized if the performance of the final model is low.

### Prediction Analysis

-   Linear regression models will estimate the impact of price changes on sales.
-   Random Forest and Logistic Regression models will classify customers based on retention likelihood.
-   The final model will be selected based on the best performance, and evaluated using R\^2 and RMSE.

### Visualization

-   Sales trends will be analyzed through different data visualizations.
-   A correlation heatmap will be used to examine relationships between pricing, sales, and ratings.
-   The most popular restaurant categories will be identified through bar charts.
-   Visualizations of attribute summaries will be created using box plots, bar charts, and histograms.
-   K-means clustering will be employed to find common traits among popular restaurants.
-   Visualizations will be created using ggplot2

## Main Features of the Application

-   **Interactive Visualizations** for sales trends, rating distributions, and cuisine popularity.
-   **Recommendation System** based on user preferences (cuisine, price range, and location).
-   **Predictive Analytics** to estimate the impact of pricing on sales and classify customer retention likelihood.
-   **Market Insights** through clustering and PCA analysis to determine key success factors for restaurants.

By combining advanced analytics with an intuitive user interface, this Shiny application will serve as a valuable tool for business owners, market analysts, and food enthusiasts looking to make data-driven decisions in the restaurant industry.

