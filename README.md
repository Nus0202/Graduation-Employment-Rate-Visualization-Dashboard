# Graduation Employment Rate Visualization Dashboard

This project is an interactive R Shiny dashboard designed to visualize employment trends among recent graduates in Canada, with a focus on helping students make informed career and academic decisions.

## 🔍 Motivation

In today's competitive job market, understanding employment outcomes is crucial—especially for students in technology and science. This dashboard aims to support:

- **Students**: to compare employment rates, salaries, and satisfaction across programs and regions.
- **Undecided students**: to choose a major based on job outcomes.
- **Educators and policymakers**: to evaluate how educational programs relate to employment trends.

## 💡 Key Features

- **Interactive Visualizations**:
  - Employment rates, salary distributions, and job satisfaction.
  - Filters by education level, region, program, and job status.
- **User-Friendly Interface**:
  - Built with `R Shiny`, styled using `shinydashboard`.
  - Plots rendered with `ggplot2` and made interactive using `plotly`.
- **Cleaned and Grouped Data**:
  - Education grouped (e.g., College/CEGEP, Bachelor, Master/PhD).
  - Regions simplified to Atlantic, Quebec, Ontario, and Western.

## 🧪 Technologies Used

- `R`
- `shiny`, `shinydashboard`
- `ggplot2`, `plotly`, `dplyr`

## ⚠️ Limitations

- **Limited Time Range**: The dashboard only includes data from 2015 and 2020 (due to Statistics Canada's availability).
- **Missing Geographical Map**: An interactive map was planned but not implemented due to technical complexity.
- **Not Web-Hosted**: Currently run locally via RStudio.

## 🚀 Future Improvements

- Add **line charts** to show multi-year trends.
- Implement an **interactive map** of Canada for regional analysis.
- Deploy the app online and enable **automatic data updates**.

## 📁 Project Structure

```bash
.
├── app.R                  # Main Shiny app script
├── data/                 # Cleaned and processed datasets
├── www/                  # (Optional) CSS or media assets
├── README.md             # Project overview
└── report/Final_report.docx # Detailed project report
