## Supplement

### Structure

``` bash
├─code
│      main_model.r
│      r0modified.py
│
└─data
        airpassenger.xlsx
        all_save_data.zip
        areacode_selected.xlsx
        contribution_results.zip
        New_cases_csv.zip
        population_density.xlsx
        UVprocessed.csv
        weather_by_area.zip
```

### Explanation

#### Code

1. `main_model.r`: calculate the contribution of factors based on our model
2. `r0modified.py`: calculate the R0 of 4 virus

#### Data

1. `airpassenger.xlsx`: the data of flight travelling
2. `all_save_data.zip`: the summary of all variables
3. `areacode_selected.xlsx`: the areas and their codes of UK
4. `contribution_results.zip`: the contribution results of weather and travelling factors in each areas
5. `New_cases_csv.zip`: the new cases of 4 virus in each areas, accumulating over time
6.  `population_density.xlsx`: the population density of each area
7.  `UVprocessed.csv`: the instensity of ultraviolet ray across nation
8.  `weather_by_area.zip`: the weather(temperature, humidity ,wind speed, pressure) of each area

### Getting Started

1. Download our code and data from this repository
2. Unzip all the `.zip` data at the same directory
3. Run `r0modified.py` to get R0 of 4 virus. (If needed, we have got the result in `all_save_data`)
   ``` python
   python code/r0modified.py --datapath data/New_cases_csv
   ```
4. Run `main_model.r` to calculate contributions. The code will go through all data in `all_save_data` and return a result file named `af_allcity.csv` under `finalresult` directory.