from sys import api_version
import pandas as pd
import os

def load_data(file_dir):
    file_set = []
    for files in os.walk(file_dir, topdown=False):
        for file in files:
            for x in file:
                if len(x) > 1:
                    file_set.append(x)
    i=0
    for file in file_set:
        df_0=pd.read_csv(f'BNU-MathModeling-2022\Final_factor_288\\area_Third_dose\\{file}',index_col='date')
        try:
            df = pd.read_csv(f"BNU-MathModeling-2022\Final_factor_288\weather_by_area\\{file.split('.')[0].title()+'.csv'}",index_col='Date')
        except FileNotFoundError:
            print(file.title()+'.csv')
            continue
        df['cumThirdDose']=df_0['cumPeopleVaccinatedThirdInjectionByVaccinationDate']
        df.to_csv(f"BNU-MathModeling-2022\Final_factor_288\weather_by_area\\{file.split('.')[0].title()+'.csv'}")
        i+=1
        print(file, "Success",f'{i}/288')

load_data(r'BNU-MathModeling-2022\Final_factor_288\\area_First_dose')
        # df['R0orig']=df_0['R0orig']
        # df['R0alp']=df_0['R0alp']
        # df['R0dlt']=df_0['R0dlt']
        # df['R0omi']=df_0['R0omi']
        # df=pd.read_csv(f"{file_dir}\{file}")
        # df['pororig']=df_0['pororig']
        # df['poralp']=df_0['poralp']
        # df['pordlt']=df_0['pordlt']
        # df['poromi']=df_0['poromi']
        # df['original']=df['newCasesBySpecimenDate']*df['pororig']
        # df['alpha']=df['newCasesBySpecimenDate']*df['poralp']
        # df['delta']=df['newCasesBySpecimenDate']*df['pordlt']
        # df['omicron']=df['newCasesBySpecimenDate']*df['poromi']
        # df['Original']=df['original'].cumsum()
        # df['Alpha']=df['alpha'].cumsum()
        # df['Delta']=df['delta'].cumsum()
        # df['Omicron']=df['omicron'].cumsum()
        # df['cumCases']=df['Orignal']+df['Alpha']+df['Delta']+df['Omicron']
        # df['cumDeaths']=df_0['mu']*df['cumCases']
        # df.to_csv(f"C:\\Users\\13955\\gitee\\Ruize_repository\\BNU-MathModeling-2022\\data\\data\\379_area_New_cases_csv\\{file}")