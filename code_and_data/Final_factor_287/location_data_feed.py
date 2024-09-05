from cvxpy import length
import pandas as pd
import os
from scipy import stats

file_dir=r"BNU-MathModeling-2022\Origin_Alpha_dataset"
location=pd.read_csv(r"BNU-MathModeling-2022\Final_factor_287\location.csv",index_col="area")

file_set = []
for files in os.walk(file_dir, topdown=False):
    for file in files:
        for x in file:
            if len(x) > 1:
                file_set.append(x)
i=0
for file in file_set[:241]:
    df=pd.read_csv(f'BNU-MathModeling-2022\Origin_Alpha_dataset\\{file}',index_col='Date')
    area_name=file.split(".")[0]
    area_latitude=float(location.loc[f'{area_name}'][2])
    area_longitude=float(location.loc[f'{area_name}'][1])
    latitude=stats.norm.rvs(area_latitude,0.0003**2,size=len(df))
    longitude=stats.norm.rvs(area_longitude,0.0003**2,size=len(df))
    df["latitude"]=latitude
    df["longitude"]=longitude
    del df["male"]
    del df["female"]
    df.to_csv(f"BNU-MathModeling-2022\Origin_Alpha_dataset\\{file}")
    i+=1
    print(f"Success!  {i}/{len(file_set)}")


    


