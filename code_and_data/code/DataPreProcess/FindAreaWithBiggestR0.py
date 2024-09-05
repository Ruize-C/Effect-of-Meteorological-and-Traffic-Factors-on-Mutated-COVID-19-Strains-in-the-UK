import pandas as pd
import os
import numpy as np


def load_data(file_dir):
    file_set = []
    for files in os.walk(file_dir, topdown=False):
        for file in files:
            for x in file:
                if len(x) > 1:
                    file_set.append(x)
    i = 0
    area_p={}
    for file in file_set:
        strain_R0={}
        df = pd.read_csv(f"{file_dir}\\{file}")
        ROorig=np.mean(df[(41 <= df.index <= 128) & (0<= df["R0orig"] <= 5)])
        ROalp=np.mean(df[(245 <= df.index <= 347) & (0<= df["R0alp"] <= 8)])
        ROdlt=np.mean(df[(417 <= df.index <= 128) & (0<= df["R0dlt"] <= 539)])
        ROomi=df["R0omi"][634:785]