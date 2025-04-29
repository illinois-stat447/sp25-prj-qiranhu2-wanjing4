# Anime Statistics and Analysis Platform (ASAP)

## Overview

Analysis of the trend of anime from the 1990s to 2020s and anime recommendation system to users.

## Authors

Qiran Hu ([qiranhu2\@illinois.edu](mailto:qiranhu2@illinois.edu))\
Zoey Yao ([wanjing4\@illinois.edu](mailto:wanjing4@illinois.edu))

## Project Proposal

This project aims to develop a Shiny website application that integrates with the anime Data API to analyze the popularity trend of anime. Interactive visualizations and prediction analysis will be implemented. The application will provide valuable insights for researchers and the general public, enabling them to better understand public's taste on anime.

## Research Question

The project will also answer the questions below

1.  Are the highest-rated anime also the most widely watched?

    -   Do anime with top user scores also have the largest number of viewers/members
        -   Are there highly rated "hidden gem" series that relatively few people have seen?
    -   Conversely, are some extremely popular titles only moderately rated?

2.  What do the most "favorite" anime have in common?

    -   Are fans marking their favorites usually the highest-scoring and most popular ones?

    -   Do certain genres or unique content qualities inspire a disproportionately high number of favorites even if those anime aren't at the very top of the popularity charts?

3.  Which studios or producers consistently produce top-performing anime?

    -   Do certain studios have a track record of anime with high average scores or huge popularity?

        -   How can we efficiently identify these could highlight which production teams repeatedly create hits.

4.  Does the score of an anime's influence its popularity?

    -   Are higher rated animes tend to earn higher popularity or attract more members or vice versa?

## Summary of Insights

-   more to add
-   more to add
-   example: Anime who are in Action genre is most popular

## Data

### Data Source

Through the [Jikan REST API](https://jikan.moe/), we can obtain the most updated data from the website, ensuring our application provides accurate and real-time information for all users.

### Key Dimensions

-   mal_id - Unique anime ID for each anime
-   title - name of the anime
-   type - TV, Movie, OVA
-   episodes - number of episodes for each anime
-   status - if the anime is currently airing or finished airing
-   rating - the rating of each anime such as PG13, R+, and PG-Children
-   score - the score of the anime based on a scale of 0-10
-   scored_by - number of users who scored the anime
-   rank - the rank of anime based on score
-   popularity - how popular is the anime
-   members - number of users who have watched the anime
-   favorites - number of users who added this anime to their favorites
-   genres - the specific style of anime
-   studios - the studios that made the anime
-   producers - the producers that made the anime

## Methodology

### Data Preprocessing

-   Data will be scraped from the web API using SQL
-   ANOVA test is used on multi-valued categorical data to find which value we should add weight when doing prediction analysis
-   Attributes with less than or equal to 10% missing values will be filled with median/average for continuous variables and "NA" with categorical variables

### Prediction Analysis

-   Lasso, Ridge, SVM, Random Forest, and Decision Tree will be built
-   The best model will be chosen

### Visualization

-   Anime trends will be analyzed through different data visualizations.
-   Visualizations of attribute summaries will be created using box plots, bar charts, and histograms.
-   Visualizations will be created using ggplot2
-   More to add

## Main Features of the Application

-   **Interactive Visualizations** for anime trends according to different attributes.
-   After the user entered their favorite anime, the recommendations of anime they have not seen will be shown
