from json import load
import os
from openpyxl import load_workbook

def smallest_set_in_fold():
    file_set_1 = []
    file_set_2 = []
    dif=[]
    for files in os.walk('BNU-MathModeling-2022\Final_factor\\area_Second_dose', topdown=False):
        for file in files:
            for x in file:
                if len(x) > 1:
                    x=x.split('.')
                    file_set_1.append(x[0].title()+".csv")
    for files in os.walk('BNU-MathModeling-2022\Final_factor\\weather_by_area', topdown=False):
        for file in files:
            for x in file:
                if len(x) > 1:
                    file_set_2.append(x)
    if len(file_set_1)>=len(file_set_2):
        for x in file_set_1:
            if x not in file_set_2:
                dif.append(x)
    else:
        for x in file_set_2:
            if x not in file_set_1:
                dif.append(x)
    i,j,k=0,0,0
    for x in dif:
        print(x)
        i+=1
        if x in file_set_1:
            j+=1
        if x in file_set_2:
            k+=1
    print(i,j,k)
def smallest_set_of_fold_and_table():
    file_set_1 = []
    file_set_2 = []

    for files in os.walk('BNU-MathModeling-2022\Final_factor\\weather_by_area', topdown=False):
        for file in files:
            for x in file:
                if len(x) > 1:
                    file_set_2.append(x)
    file=load_workbook(r"BNU-MathModeling-2022\Final_factor\areacode_selected.xlsx")
    sheet=file.worksheets[0]
    for row in sheet.rows:
        x=row[1].value.split('.')[0]
        file_set_1.append(x.title()+".csv")
    i=0
    for x in file_set_2:
        if x not in file_set_1:
            os.remove(f'BNU-MathModeling-2022\Final_factor\\weather_by_area\\{x}')
            i+=1
            print(i)
smallest_set_of_fold_and_table()    

    
