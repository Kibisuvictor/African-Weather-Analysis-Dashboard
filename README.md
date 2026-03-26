# African-Weather-Analysis-Dashboard
An interactive R Shiny dashboard designed to explore, visualize, and analyze weather patterns across the African continent using the Global Weather Repository. This project implements advanced data science techniques, including dimensionality reduction (t-SNE/UMAP) and network analysis, to uncover climatic similarities between regions.

🚀 Features
Regional Overview: Interactive heatmaps and distribution plots summarizing weather metrics by subregion and country.

High-Dimensional Analysis: Explore complex weather clusters using t-SNE and UMAP algorithms, with dynamic controls for perplexity and color mapping.

Similarity Networks: A graph-based view of African capitals based on Euclidean distance of weather conditions, featuring Louvain Community Detection.

Temporal Trends: Animated bubble plots (via gganimate) showing the evolution of temperature, humidity, and wind over time.

Case Study: A deep-dive network analysis of Eastern African capitals specifically for April 1st, 2025.

Data Explorer: A granular filtering system with an interactive DT table for raw data inspection.

🛠️ Tech Stack
Language: R

Framework: Shiny, shinydashboard

Data Manipulation: tidyverse, lubridate, janitor

Visualization: plotly, ggplot2, viridis, ggrepel

Advanced Analytics: Rtsne, uwot (UMAP), igraph

Animation: gganimate, gifski

📦 Installation & Setup
Clone the repository:

Bash
git clone https://github.com/your-username/african-weather-dashboard.git
cd african-weather-dashboard
Ensure you have the dataset:
Place the GlobalWeatherRepository.csv file in the root directory of the project.

Install dependencies:
Open R and run:

R
install.packages(c("shiny", "shinydashboard", "tidyverse", "lubridate", 
                   "plotly", "janitor", "Rtsne", "uwot", "igraph", 
                   "gganimate", "ggrepel", "viridis", "DT", "shinycssloaders"))
Run the Application:

R
shiny::runApp()
📊 Methodology
Dimensionality Reduction
To visualize the 7-dimensional weather space (Temperature, Pressure, Humidity, Precipitation, Cloud Cover, Wind Degree, and Wind Speed), the app scales the data and projects it into 2D space using t-SNE or UMAP. This allows for the identification of weather "regimes" across different countries.

Network Analysis
The "Similarity Network" treats each city as a node. Edges are created between cities whose weather profiles are within a specific Euclidean distance threshold. The Louvain algorithm is then applied to identify clusters of cities with statistically similar climates.

📁 Project Structure
app.R: The main script containing both UI and Server logic.

GlobalWeatherRepository.csv: The source dataset (ensure this is included or linked).

www/: Directory containing generated animations and static assets.

README.md: Project documentation.

📝 License
Distributed under the MIT License. See LICENSE for more information.
