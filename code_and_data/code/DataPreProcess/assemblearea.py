import numpy as np
import os
import pandas as pd

df = pd.DataFrame(columns=["cityname","Temperature (°F)","Humidity (%)","Wind Speed (mph)","Pressure (inHg)"])
rootpth = r"BNU-MathModeling-2022\data\ExtraData\weather_by_area"
areas = os.listdir(rootpth)
i=1
for area in areas:
    areadf = pd.read_csv(os.path.join(rootpth,area),usecols=["Temperature (°F)","Humidity (%)","Wind Speed (mph)","Pressure (inHg)"])
    areaname = area[:-4]
    R0orig = np.mean(areadf["Temperature (°F)"])
    R0alp  = np.mean(areadf["Humidity (%)"])
    R0dlt  = np.mean(areadf["Wind Speed (mph)"])
    R0omi  = np.mean(areadf["Pressure (inHg)"])
    addlist = [areaname,R0orig,R0alp,R0dlt,R0omi]
    # print(addlist)
    df.loc[i] = addlist
    i+=1

df.to_csv(r"BNU-MathModeling-2022\data\ExtraData\assembleWeather.csv")