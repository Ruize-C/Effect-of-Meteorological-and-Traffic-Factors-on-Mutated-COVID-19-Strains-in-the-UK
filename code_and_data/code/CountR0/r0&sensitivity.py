#灵敏度分析与R0统计

import os
import pandas as pd
import numpy as np
import warnings
import csv

def findduandian(colname, para, long):
    '''
    Find interval 
    Input: column name, card parameter C 
    Output: two endpoints
    '''
    start = 0
    end = 0
    for i in range(long):
        if rawdf[colname][i] > para:
            start = i
            break
    for j in range(long-1,0,-1):
        if rawdf[colname][j] > para:
            end = j
            break
    return (start,end)

def recnormalize(mean, std ,long):
    #count the normal distribution (math.)
    x = np.arange(0,long)
    y = (1/((2*3.14159)**0.5*std))*np.exp(-((x-mean)**2/(2*std**2)))
    return y

def getr0(recover,fatality,suspect,infect):
    #Calculate R0 and filter out outliers
    di = np.zeros(len(infect)-1)
    ds = np.zeros(len(suspect)-1)
    for i in range(1,len(infect)):
        di[i-1] = infect[i]-infect[i-1]
        ds[i-1] = suspect[i]-suspect[i-1]
    r0 = (di-ds-fatality[1:]*suspect[1:]+fatality[1:])/((recover[1:]+fatality[1:])*suspect[1:]*infect[1:]*2)+1/(2*suspect[1:])
    #r0=(di+(recover[1:]+fatality[1:])*infect[1:])/((recover[1:]+fatality[1:])*suspect[1:]*infect[1:])
    r0 = np.array(r0)
    
    for i in range(len(r0)):
        if r0[i] <0:
            r0[i]=0
    return r0

def getdeath(para, long):
    '''
    Calculate mortality rate 
    Input Card endpoint parameter C and length of time 
    Output Four virus cure rates (1 - mortality rate) and four virus endpoints
    '''

    #The "por-" column here is the ratio obtained by subtracting the daily additions from the current day additions.
    #Find the four endpoints first.
    origs,orige = findduandian("pororig",para,long)
    alps, alpe = findduandian("poralp", para, long)
    dlts, dlte = findduandian("pordlt", para,long)
    omis, omie = findduandian("poromi", para,long)
    print(origs,orige)
    print(alps,alpe)
    print(dlts,dlte)
    print(omis,omie)
    #Calculate the number of deaths for each virus by taking the total number of deaths and dividing it into four periods according to the duration of the pandemic.

    cumudeath["Original"] = cumudeath["cumDeaths"][origs:orige]
    for i in range(orige,long):
        cumudeath["Original"][i]=int(cumudeath["cumDeaths"][orige-1])

    cumudeath["Alpha"] = cumudeath["cumDeaths"][alps:alpe]
    for i in range(alps,alpe):
        cumudeath["Alpha"][i]-=int(cumudeath["cumDeaths"][alps-1])
    for i in range(alpe,long):
        cumudeath["Alpha"][i]=int(cumudeath["cumDeaths"][alpe-1])
    
    cumudeath["Delta"] = cumudeath["cumDeaths"][dlts:dlte]
    for i in range(dlts,dlte):
        cumudeath["Delta"][i]-=int(cumudeath["cumDeaths"][dlts-1])
    for i in range(dlte,long):
        cumudeath["Delta"][i]=int(cumudeath["cumDeaths"][dlte-1])
    
    cumudeath["Omicron"] = cumudeath["cumDeaths"][omis:omie]
    for i in range(omis,long):
        cumudeath["Omicron"][i]-=int(cumudeath["cumDeaths"][omis-1])
    cumudeath.fillna(0,inplace=True)
    #The number of deaths from the virus needs to be subtracted from the previously added base to obtain the death rate, which is compared to the total number of deaths.
    orig = (cumudeath["Original"][orige-1]-cumudeath["Original"][origs])/(cumbingli["Original"][orige-1]-cumbingli["Original"][origs])
    alp = (cumudeath["Alpha"][alpe-1]-cumudeath["Alpha"][alps])/(cumbingli["Alpha"][alpe-1]-cumbingli["Alpha"][alps])
    dlt = (cumudeath["Delta"][dlte-1]-cumudeath["Delta"][dlts])/(cumbingli["Delta"][dlte-1]-cumbingli["Delta"][dlts])
    omi = (cumudeath["Omicron"][omie-1]-cumudeath["Omicron"][omis])/(cumbingli["Omicron"][omie-1]-cumbingli["Omicron"][omis])
    return (1-orig),(1-alp),(1-dlt),(1-omi),origs,orige,alps,alpe,dlts,dlte,omis,omie

def getrecover(origrate,alprate,dltrate,omirate, long):
    '''
    Calculate the number of cures Return the cure status of the four viruses over time 
    Input the cure rate of the four viruses and the length of time 
    Output the number of cures of the four viruses
    '''
    rt0 = np.zeros(long,dtype=int)
    rta = np.zeros(long,dtype=int)
    rtd = np.zeros(long,dtype=int)
    rto = np.zeros(long,dtype=int)
    for t in range(long):
        for n in range(t):
            rt0[t] += int(rawdf["original"][n]*origin[t-n]*origrate)
    for t in range(long):
        for n in range(t):
            rta[t] += int(rawdf["alpha"][n]*alpha[t-n]*alprate)
    for t in range(long):
        for n in range(t):
            rtd[t] += int(rawdf["delta"][n]*delta[t-n]*dltrate)
    for t in range(long):
        for n in range(t):
            rto[t] += int(rawdf["omicron"][n]*omicron[t-n]*omirate)

    return rt0,rta,rtd,rto
    
if __name__ == "__main__":
    testname = "gengxin1"

    warnings.filterwarnings("ignore")
    rawdf = pd.read_csv(r"BNU-MathModeling-2022/processed/adddata2.CSV",usecols=["date","original","alpha","delta","omicron","pororig","poralp","pordlt","poromi"])      #四个病毒的日新增病例 以及与总新增作比的比例 （差分）
    cumbingli = pd.read_csv(r"BNU-MathModeling-2022/processed/cumnum2.CSV",usecols=["date","Original","Alpha","Delta","Omicron"])     #四个病毒的总新增
    cumudeath = pd.read_csv(r"BNU-MathModeling-2022/processed/dailydeath2.CSV",usecols=["date","cumDeaths"])       #四个病毒的总死亡

    long = len(rawdf)
    origin = recnormalize(13.4,4.94,long)
    alpha = recnormalize(11.7,3.82,long)
    delta = recnormalize(10.9,6.97,long)
    omicron = recnormalize(9.87,5.23,long)

    # r0mat = np.zeros((4,6))

    para = 0.8             #This is parameter C. See how much you want to jam.
    a,b,c,d,origs,orige,alps,alpe,dlts,dlte,omis,omie = getdeath(para,long)
    rt0,rta,rtd,rto = getrecover(a,b,c,d,long)

    cumulateori= np.zeros(long)
    cumulateori[0] = rt0[0]
    for i in range(1,long):
        cumulateori[i] = cumulateori[i-1]+rt0[i]
    cumulatealp= np.zeros(long)
    cumulatealp[0] = rta[0]
    for i in range(1,long):
        cumulatealp[i] = cumulatealp[i-1]+rta[i]
    cumulatedlt= np.zeros(long)
    cumulatedlt[0] = rtd[0]
    for i in range(1,long):
        cumulatedlt[i] = cumulatedlt[i-1]+rtd[i]
    cumulateomi= np.zeros(long)
    cumulateomi[0] = rto[0]
    for i in range(1,long):
        cumulateomi[i] = cumulateomi[i-1]+rto[i]
    population = np.ones(long,dtype=int)*66834405

    muorig = cumudeath["Original"]/cumbingli["Original"]
    mualp = cumudeath["Alpha"]/cumbingli["Alpha"]
    mudlt = cumudeath["Delta"]/cumbingli["Delta"]
    muomi = cumudeath["Omicron"]/cumbingli["Omicron"]
    gamaorig = cumulateori/cumbingli["Original"]
    gamaalp = cumulatealp/cumbingli["Alpha"]
    gamadlt = cumulatedlt/cumbingli["Delta"]
    gamaomi = cumulateomi/cumbingli["Omicron"]
    # print(cumbingli["Original"].shape,cumulateori.shape,cumudeath["Original"].shape)
    Iorig =  (cumbingli["Original"]-cumulateori-cumudeath["Original"])/population
    Ialp = (cumbingli["Alpha"]-cumulatealp-cumudeath["Alpha"])/population
    Idlt = (cumbingli["Delta"]-cumulatedlt-cumudeath["Delta"])/population
    Iomi = (cumbingli["Omicron"]-cumulateomi-cumudeath["Omicron"])/population
    Sorig = (population-cumbingli["Original"])/population
    Salp = (population-cumbingli["Alpha"])/population
    Sdlt = (population-cumbingli["Delta"])/population
    Somi = (population-cumbingli["Omicron"])/population

    # origr0 = getr0(gamaorig,muorig,Sorig,Iorig)
    # r0mat[0,k] = np.mean(origr0[origs+100:orige])
    # alpr0 = getr0(gamaalp,mualp,Salp,Ialp)
    # r0mat[1,k] = np.mean(alpr0[alps:alpe])
    # dltr0 = getr0(gamadlt,mudlt,Sdlt,Idlt)
    # r0mat[2,k] = np.mean(dltr0[dlts:dlte])
    # omir0 = getr0(gamadlt,mudlt,Sdlt,Idlt)
    # r0mat[3,k] = np.mean(omir0[omis:omie])

        # r0m = pd.DataFrame(r0mat)
        # r0m.to_csv("r0.csv")


    origr0 = getr0(gamaorig,muorig,Sorig,Iorig)
    with open("BNU-MathModeling-2022/processed/para{}_{}/origr0.csv".format(para,testname),'w',newline='') as f:
        for i in origr0:
            tup = [i]
            writer = csv.writer(f)
            writer.writerow(tup)
    alpr0 = getr0(gamaalp,mualp,Salp,Ialp)
    with open("BNU-MathModeling-2022/processed/para{}_{}/alpr0.csv".format(para,testname),'w',newline='') as f:
        for i in alpr0:
            tup = [i]
            writer = csv.writer(f)
            writer.writerow(tup)
    dltr0 = getr0(gamadlt,mudlt,Sdlt,Idlt)
    with open("BNU-MathModeling-2022/processed/para{}_{}/dltr0.csv".format(para,testname),'w',newline='') as f:
        for i in dltr0:
            tup = [i]
            writer = csv.writer(f)
            writer.writerow(tup)
    omir0 = getr0(gamadlt,mudlt,Sdlt,Idlt)
    with open("BNU-MathModeling-2022/processed/para{}_{}/omir0.csv".format(para,testname),'w',newline='') as f:
        for i in dltr0:
            tup = [i]
            writer = csv.writer(f)
            writer.writerow(tup)