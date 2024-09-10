# Russian State Media Article Parser

## Overview
This R script processes and organizes data from Russian state media articles. It extracts information such as media names, publication dates, titles, and rubrics from text files, cleans the data, and outputs it to an Excel file.

## Prerequisites
- R 4.x
- Required R packages: `tidyverse`, `stringr`, `writexl`, `openxlsx`

## Setup

1. **Install Required Packages**:
   Ensure the necessary packages are installed by running:
   ```r
   install.packages(c("tidyverse", "stringr", "writexl", "openxlsx"))
   ```

2. **Input Files**:
   - Place your text files in the specified directory: `/Users/mac12/Downloads/ЮАР/final/`.
   - File names should correspond to the entries in the `countries2` vector.

## Functions

- `get_media(x)`: Extracts the name of the media outlet from the article text.
- `get_date(x)`: Extracts the publication date from the article text.
- `get_title(x)`: Extracts the article title from the text.
- `get_df(string)`: Reads and splits text files into separate strings based on the source.
- `add_df_rows(countries, df)`: Adds rows from text files for specified countries to the dataframe.
- `check_countries(x)`: Flags occurrences of specific countries in the text.
- `get_rubric(df)`: Extracts and cleans the rubric/category from the text.
- `prepare_dataset(countries, df)`: Processes and organizes the dataset, including extracting relevant information and handling text formatting.

## Usage

1. **Prepare the Dataset**:
   - Modify the `countries2` vector to include the countries you are analyzing.
   - Run the script to process and structure the data.

2. **Run the Script**:
   Execute the R script to clean and organize the data:
   ```r
   source("path_to_your_script.R")
   ```

## Output

- The script outputs the cleaned and structured data to an Excel file named `Articl1.xlsx` located in `/Users/mac12/Downloads/ЮАР/final/`.

## Notes

- Ensure your text files are properly formatted and placed in the designated directory.
- Adjust the `countries2` vector and file paths as needed to fit your dataset.
