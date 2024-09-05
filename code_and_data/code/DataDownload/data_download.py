from openpyxl import load_workbook
import wget
import os
import shutil
import time

def mycopyfile(srcfile,dstpath):                       # copy function
    if not os.path.isfile(srcfile):
        print ("%s not exist!"%(srcfile))
    else:
        fpath,fname=os.path.split(srcfile)             # Separate file names and paths
        if not os.path.exists(dstpath):
            os.makedirs(dstpath)                       # Create a path
        shutil.copy(srcfile, dstpath + fname)          # Copy the file os.remove(srcfile)

area_code=load_workbook(r"BNU-MathModeling-2022\code\area_code.xlsx")
sheet = area_code.worksheets[0] 
i=0
for row in sheet.rows:
    if row[2].value!="LAD20NM":
        url=f"https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&areaCode={row[1].value}&metric=cumPeopleVaccinatedThirdInjectionByVaccinationDate&format=csv"
        dst=r"BNU-MathModeling-2022\data\ExtraData\area_Third_dose\\"
        wget.download(url,dst)
        os.rename(r"BNU-MathModeling-2022\data\ExtraData\area_Third_dose\ltla_2022-08-03.csv",f"BNU-MathModeling-2022\data\ExtraData\\area_Third_dose\{row[2].value}.csv")
        i+=1
        print(f" | {row[2].value}.csv下载成功!  {i}/379")
        time.sleep(0.5)
