# SonicPlots

**SonicPlots** is an experimental data visualisation project using sonification — transforming data into sound — to enhance accessibility and provide new perspectives on interpreting information.

This project was developed for my MSc Applied Data Science & Statistics dissertation at the University of Exeter, completed in 2024.

### ***Code can be found here:  inst / app / app.R***

## Project Overview

The core of SonicPlots is a prototype web application built in **R** using the **Shiny** framework. It allows users to:

✅ Upload tabular datasets (e.g., CSV files)  
✅ Generate simple auditory mappings of data columns  
✅ Explore how pitch, volume, and duration can represent data trends  
✅ Combine sonification with basic visual plots for an accessible, multi-sensory experience  

> **Note:** The web application is not currently hosted online due to hosting costs. I am actively working on a standalone desktop version to broaden accessibility.

## Installation

Install with:
``` r
remotes::install_github("lukew1357/SonicPlots")
```

## Running the application

Run with:
```{r}
library(SonicPlots)

SonicPlots::runApp()
```

## Technologies & Tools

- **R** programming language
- **Shiny** for web application development
- **RStudio** IDE for development
- **Python** for prototyping and proof of concept

This application makes use of the following libraries: `lubridate`, `dplyr`, `tuneR`, `forecast`, `fluidsynth` and `future`

## Timeline

- Project developed between February 2024 - July 2024  
- Dissertation submitted July 2024  

## Dissertation Document

The accompanying dissertation document explaining the research process, methodology, and findings is available upon request. Please contact me directly if you'd like to read it.

## Future Work

- Develop a standalone desktop version (in progress)  
- Improve the flexibility of sonification mappings  
- Explore more complex audio synthesis options  

## Contact

Feel free to explore the code and reach out with questions or suggestions.

GitHub: [lukew1357](https://github.com/lukew1357)  
Email: williams.luke999@gmail.com

---

> ⚠️ This project is protected by copyright. No use, distribution, or modification is permitted without the author's explicit permission.

---
