# README

---

### Code&Data

```
code_and_data
├─code          #code
│      pretreatment.ipynb       #Data preprocessing
│      r0&sensitivity.py        #Calculate R0 and verify C sensitivity
│      pretreatment.ipynb       #Data preprocessing
│      prop2cases.ipynb         #Proportion of cases transferred
│      separatevariant.ipynb    #Calculation of data required for each case
│      daliyselect.ipynb        #Interpolation by date
│      fetch_weather.py         #crawl data
│
├─data          #raw data
│      adddata.csv              #Data on the number of new cases per case day and the percentage of new cases per day
│      alpha.CSV                #alpha census data
│      cumnum.CSV               #Total new data by case
│      death.CSV                #Total death data for all cases
│      delta.CSV                #delta census data
│      omicron.CSV              #omicron census data
│
└─processed     #Processed data
    │  diff_omicron.csv         #Processed omicron differential data
    │  sum_omicron.csv          #Processed omicron summation data
    │  proportion.csv           #Data on the daily proportion of each virus
    │  variantdeath.csv         #Same-day mortality for each virus
    │  variantnewcases.csv      #Number of new cases of each virus
    │
    └─para0.85                  #Example, R0 for each virus at C=0.85
            alpr0.csv
            dltr0.csv
            omir0.csv
            origr0.csv
    └─delta                     #Data on delta

```

#### Instructions for use

1. **pretreatment** is responsible for integrating the pre-processing of the census data of each virus by day and gives the difference and summation values of each virus after integration. If it is necessary to react to the actual situation, it is necessary to calculate a coefficient of conversion based on the number of new cases on the day according to the peak of the virus propagation, and the converted data will need to be saved again in a new place. A small amount of fine-tuning of the parameters of these runs has been noted in the notebook.
2. **r0&sensitivity** is responsible for calculating R0, three data need to be prepared: ``adddata.csv`` daily additions and percentage of each virus (need to be integrated based on the data obtained in the previous step), ``cumnum.csv`` total additions of each virus, ``death.csv`` total deaths. When you are ready, you can change C in the main function and the results will be automatically saved in the folder para_C.
