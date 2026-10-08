Amazon Video Game Reviews (Hive) & UK Road Safety Dashboard (R Shiny)
Repository: games-analysis-road-dashboard
Tools & Skills
•	Big data: Hive (HiveQL) on a Cloudera virtual machine
•	Analysis: R – tidyverse, dplyr, tidyr, janitor, lubridate, readr, readxl, ggplot2
•	Dashboard: R Shiny, shinythemes, plotly, DT
•	Skills: Data cleaning, joining datasets, feature engineering, exploratory data analysis, geospatial visualisation, interactive dashboard design
What I Did
This project was the second assignment for the Programming for Data Science & AI module of my Data Analytics MSc and is made up of three parts.
•	Amazon reviews analysis (Hive): I set up a Hive database on a Cloudera VM, created a reviews table and loaded 40,000 Amazon video game reviews. I then wrote HiveQL queries to rank the 10 most popular games (most 5-star reviews) and least popular games (most 1-star reviews), and added extra queries a product team might find useful: average rating per game, rating distribution, review volume, and the most polarising games (more than 50 1-star and 50 5-star reviews).
•	Road safety exploration (R): I cleaned and reduced the Department for Transport Road Safety Data 2021 and joined it to its metadata guide so numeric codes became readable labels. I then visualised accidents by severity and road type, weather and road surface conditions, accident density in Greater Manchester (dot map) and Devon & Cornwall (heatmap), and average casualties by number of vehicles.
•	Interactive dashboard (R Shiny): I cleaned the accidents and vehicles datasets (removing missing values, duplicates and 23 redundant columns) and left-joined vehicles to accidents to give one row per vehicle. I engineered grouped features for vehicle type, manoeuvre, skidding, hit object, driver age, vehicle age and propulsion, then built the "UK Collision Trends & Insights 2021" dashboard. It has 8 vehicle filters and 5 tabs (Overview, Severity, Time, Weather, Location), each with reactive summary tables and interactive plotly charts, including a lollipop chart, treemap, bar charts and a coordinate map.
Key Findings
•	Game B004MC8CA2 was the most popular and most reviewed game, with 1,605 five-star ratings out of 2,041 reviews. It also had 70 one-star ratings, making it the most polarising game.
•	Game B00409G750 was the least popular, with 78 one-star ratings.
•	Single carriageways had the most accidents at every severity level, likely because they are the most common and most used road type. Slight accidents outnumbered serious and fatal ones for every road type.
•	Most accidents happened on dry roads in fine weather, suggesting more people drive, and so more accidents occur, in good conditions.
•	Accident density peaked in Manchester city centre and Plymouth, the most populated urban areas in each region.
•	Average casualties peaked at around 2.6 when 8 vehicles were involved and were lowest with very few or very many vehicles, so mid-sized collisions caused the most harm per accident.
Files
•	CIS4514 coursework 2 report.docx – full report covering all three tasks, with an appendix containing the Hive code and R outputs
•	CIS4514 task 2.2.r – R script for cleaning, joining and exploring the road safety data
•	CIS4514 task 2.3.r – R Shiny script for the interactive road safety dashboard
