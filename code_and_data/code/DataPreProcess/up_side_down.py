import pandas as pd
import os

def up_side_down(dir):
    df=pd.read_csv(dir)
    df = df.sort_index(ascending=False)
    df.to_csv(dir,index=None)
    print("Success!")

file_set = []
file_dir = r'BNU-MathModeling-2022\Final_factor_287\area_PCA_dose'
for files in os.walk(file_dir, topdown=False):
    for file in files:
        for x in file:
            if len(x) > 1:
                file_set.append(x)
for file in file_set:
    up_side_down(f'{file_dir}\{file}')