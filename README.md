# Flight cancellation and delay time prediction of STAT628 group 9

## Repository Overview

This repository contains all the necessary data, code, and resources for analyzing and presenting the flights cancellation rate and delay time prediction application using a Shiny app. The project includes data cleaning, analysis scripts, image outputs, and relevant documentation.

### Directory Structure

-   **data/**: This folder contains the raw and cleaned datasets used in the analysis. Raw data is kept in its original form, while cleaned data is prepared for analysis.

-   **code/**: This folder includes all the code necessary for data cleaning, analysis, and generating visual outputs (such as tables and figures). It also contains the Shiny app code that serves as the base for the body fat prediction app.

-   **summary.pdf**: A two-page PDF summarizing the findings and key results from the analysis and prediction model.

-   **presentation_slides.pdf**: The final presentation slides providing an overview of the project, methodology, and results.

### How to Use the Code

1.  **Data Preparation**:
    -   Place the raw dataset(s) in `downloaded_data` in the `data/` folder.
    -   Make another folder `raw_data` for the decompressed flight csv files. Then run `decompression.bat` to extract the zip files.
2.  **Running the Analysis**:
    -   The `code/` folder contains scripts for analyzing the data and generating results. `data_clean.ipynb` will process the raw data and merge with the weather data downloaded from web api.
    -   `cancellation_pattern.ipynb` includes some explorative attempts of cancellation rate and delay time.
    -   If you need to modify the analysis (e.g., changing variables or models), adjust the notebook code files accordingly.
3.  **Shiny App**:
    -   The Shiny app for predicting body fat percentage is also located in the `code/flight_app/` folder. The app allows users to input data and generate predictions based on the models created.

    -   Our app is hosted here, just try it!

        <https://hyang644.shinyapps.io/flight_app/>
4.  **Viewing Results**:
    -   The final presentation slides and summary PDF are available in the root directory for easy reference.

### Additional Notes

-   **System Requirements**: This project requires R (version 3.6 or above) and the following libraries: `shiny`, `ggplot2`, `arrow`, `dplyr`, `lubridate`, `tidyr`, `corrplot`, `caret`. Python packages include: `numpy`, `pandas`, `matplotlib`, `requests` and `pytz`.
-   **Data Source**: 
    -   Flights data: https://www.transtats.bts.gov/DL_SelectFields.aspx?gnoyr_VQ=FGK&QO_fu146_anzr=b0-gvzr
    - Iowa Environment Mesonet, ASOS data download.
https://mesonet.agron.iastate.edu/request/download.phtml?network=IA_ASOS
    - Github repo:https://github.com/akrherz/iem/blob/main/pylib/iemweb/request/asos.py
-   **Custom Modifications**: If you wish to modify the models (e.g., adding more predictors or changing the analysis), adjust the code in the `code/` folder accordingly.

### Group Members

Hengyu Yang, Leyan Sun, Yi Ma, Tianle Qiu
