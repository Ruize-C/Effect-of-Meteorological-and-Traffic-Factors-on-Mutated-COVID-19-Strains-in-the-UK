import os
import pandas as pd
import numpy as np
import warnings
import csv
import argparse

class CountR0():
    def __init__(self, testname, rawdf, cumbingli, cumdeath) -> None:
        self.testname = testname
        self.rawdf = rawdf
        self.cumbingli = cumbingli
        self.cumdeath = cumdeath

        self.long = len(rawdf)
        
        print("init normal distribution")
        self.originNd = self.__recnormalize(13.4,4.94,self.long)#2.22
        self.alphaNd = self.__recnormalize(11.7,3.82,self.long)#1.95
        self.deltaNd = self.__recnormalize(10.9,6.97,self.long)#2.64
        self.omicronNd = self.__recnormalize(9.87,5.23,self.long)#2.28

        print("init recover/fatality")
        self.population = 67326569*np.ones(self.long)
        self.mu = self.cumdeath["cumDeaths"]/self.cumbingli["cumCases"]        #Total deaths/total additions TBD
        self.rt0, self.rta, self.rtd, self.rto = self.__getRecover()
        self.Rc0, self.Rca, self.Rcd, self.Rco = self.__cumRecover()

        print("init gama")
        self.gamaorig = self.Rc0/self.cumbingli["Original"]
        self.gamaalp = self.Rca/self.cumbingli["Alpha"]
        self.gamadlt = self.Rcd/self.cumbingli["Delta"]
        self.gamaomi = self.Rco/self.cumbingli["Omicron"]

        print("init I")
        self.Iorig =  (self.cumbingli["Original"]-self.Rc0-self.cumdeath["cumDeaths"]*(self.cumbingli["Original"]/self.cumbingli["cumCases"]))/self.population
        self.Ialp = (self.cumbingli["Alpha"]-self.Rca-self.cumdeath["cumDeaths"]*(self.cumbingli["Alpha"]/self.cumbingli["cumCases"]))/self.population
        self.Idlt = (self.cumbingli["Delta"]-self.Rcd-self.cumdeath["cumDeaths"]*(self.cumbingli["Delta"]/self.cumbingli["cumCases"] ))/self.population
        self.Iomi = (self.cumbingli["Omicron"]-self.Rco-self.cumdeath["cumDeaths"]*(self.cumbingli["Omicron"]/self.cumbingli["cumCases"] ))/self.population

        print("init S")
        self.Sorig = (self.population-self.cumbingli["Original"])/self.population
        self.Salp = (self.population-self.cumbingli["Alpha"])/self.population
        self.Sdlt = (self.population-self.cumbingli["Delta"])/self.population
        self.Somi = (self.population-self.cumbingli["Omicron"])/self.population

        print("init R0")
        self.R0orig = self.__getr0(self.originNd,self.mu,self.Sorig,self.Iorig)
        self.R0alp = self.__getr0(self.alphaNd,self.mu,self.Salp,self.Ialp)
        self.R0dlt = self.__getr0(self.deltaNd,self.mu,self.Sdlt,self.Idlt)
        self.R0omi = self.__getr0(self.omicronNd,self.mu,self.Somi,self.Iomi)


    def __recnormalize(self, mean, std ,long):
        '''
        get confidence interval
        '''
        x = np.arange(0,long)
        y = (1/((2*3.14159)**0.5*std))*np.exp(-((x-mean)**2/(2*std**2)))
        return y    
    
    def __getr0(self,recover,fatality,population,infect):
        '''
        get R0 and filter the outliers
        '''
        di = np.zeros(len(infect)-1)
        ds = np.zeros(len(population)-1)
        for i in range(1,len(infect)):
            di[i-1] = infect[i]-infect[i-1]
            ds[i-1] = population[i]-population[i-1]
        #r0 = (di-ds-fatality[1:]*population[1:]+fatality[1:])/((recover[1:]+fatality[1:])*population[1:]*infect[1:]*2)+1/(2*population[1:])
        r0=di/((recover[1:]+fatality[1:])*population[1:]*infect[1:])+1/population[1:]
        r0 = np.array(r0)
        
        # for i in range(len(r0)):
        #     if r0[i] <0:
        #         r0[i]=0
        return r0

    def __getRecover(self):
        '''
        count the amount of recovered patients 
        inupt: the recovery rate of 4 virus and the length of time
        output: the amount of patients for 4 virus
        '''
        rt0 = np.zeros(self.long,dtype=int)
        rta = np.zeros(self.long,dtype=int)
        rtd = np.zeros(self.long,dtype=int)
        rto = np.zeros(self.long,dtype=int)
        for t in range(self.long):
            for n in range(t):
                rt0[t] += int(self.rawdf["original"][n]*self.originNd[t-n]*(1-self.mu[n]))
        for t in range(self.long):
            for n in range(t):
                rta[t] += int(self.rawdf["alpha"][n]*self.alphaNd[t-n]*(1-self.mu[n]))
        for t in range(self.long):
            for n in range(t):
                rtd[t] += int(self.rawdf["delta"][n]*self.deltaNd[t-n]*(1-self.mu[n]))
        for t in range(self.long):
            for n in range(t):
                rto[t] += int(self.rawdf["omicron"][n]*self.omicronNd[t-n]*(1-self.mu[n]))

        return rt0,rta,rtd,rto


    def __cumRecover(self):
        '''
        get the integrated amount of recoverys
        '''
        cumulateori= np.zeros(self.long)
        cumulateori[0] = self.rt0[0]
        for i in range(1,self.long):
            cumulateori[i] = cumulateori[i-1]+self.rt0[i]
        cumulatealp= np.zeros(self.long)
        cumulatealp[0] = self.rta[0]
        for i in range(1,self.long):
            cumulatealp[i] = cumulatealp[i-1]+self.rta[i]
        cumulatedlt= np.zeros(self.long)
        cumulatedlt[0] = self.rtd[0]
        for i in range(1,self.long):
            cumulatedlt[i] = cumulatedlt[i-1]+self.rtd[i]
        cumulateomi= np.zeros(self.long)
        cumulateomi[0] = self.rto[0]
        for i in range(1,self.long):
            cumulateomi[i] = cumulateomi[i-1]+self.rto[i]

        return cumulateori, cumulatealp, cumulatedlt, cumulateomi

    def save(self,path):
        '''
        save the results
        '''
        print("start save r0")
        pth = os.path.join(path,self.testname)
        if not os.path.isdir(pth):
            os.mkdir(pth)
        with open(os.path.join(pth,"origr0.csv"),'w',newline='') as f:
            for i in self.R0orig:
                tup = [i]
                writer = csv.writer(f)
                writer.writerow(tup)
        with open(os.path.join(pth,"alpr0.csv"),'w',newline='') as f:
            for i in self.R0alp:
                tup = [i]
                writer = csv.writer(f)
                writer.writerow(tup)
        with open(os.path.join(pth,"dltr0.csv"),'w',newline='') as f:
            for i in self.R0dlt:
                tup = [i]
                writer = csv.writer(f)
                writer.writerow(tup)
        with open(os.path.join(pth,"omir0.csv"),'w',newline='') as f:
            for i in self.R0omi:
                tup = [i]
                writer = csv.writer(f)
                writer.writerow(tup)
        print("finished!")
        return

def load_data(file_dir):
    '''
    load data from
    '''
    file_set = []
    for files in os.walk(file_dir, topdown=False):
        for file in files:
            for x in file:
                if len(x) > 1:
                    file_set.append(x)
    i=0
    for file in file_set:
        df=pd.read_csv(f"{file_dir}\\{file}")
        name=1
        rawdf = pd.read_csv(f"{file_dir}\\{file}",usecols=["date","original","alpha","delta","omicron","pororig","poralp","pordlt","poromi"])      #四个病毒的日新增病例 以及与总新增作比的比例 （差分）
        cumbingli = pd.read_csv(f"{file_dir}\\{file}",usecols=["date","Original","Alpha","Delta","Omicron","cumCases"])     #四个病毒的总新增
        cumdeath = pd.read_csv(f"{file_dir}\\{file}",usecols=["date","cumDeaths"])       #四个病毒的总死亡

        countr0 =CountR0(name , rawdf, cumbingli, cumdeath)
        df['R0orig']= pd.Series(countr0.R0orig)
        df['R0alp']= pd.Series(countr0.R0alp)
        df['R0dlt']= pd.Series(countr0.R0dlt)
        df['R0omi']= pd.Series(countr0.R0omi)
        df.to_csv(f"{file_dir}\\{file}")
        i+=1
        print(file, "Success",f'{i}/23')

if __name__  == "__main__":

    # name = "classtest3"
    warnings.filterwarnings("ignore")
    # rawdf = pd.read_csv(r"BNU-MathModeling-2022/processed/adddata3.CSV",usecols=["date","original","alpha","delta","omicron","pororig","poralp","pordlt","poromi"])      #四个病毒的日新增病例 以及与总新增作比的比例 （差分）
    # cumbingli = pd.read_csv(r"BNU-MathModeling-2022/processed/cumnum2.CSV",usecols=["date","Original","Alpha","Delta","Omicron","cumCases"])     #四个病毒的总新增
    # cumdeath = pd.read_csv(r"BNU-MathModeling-2022/processed/dailydeath2.CSV",usecols=["date","cumDeaths"])       #四个病毒的总死亡

    # countr0 =CountR0(name , rawdf, cumbingli, cumdeath)
    # countr0.save(r"BNU-MathModeling-2022/processed/")
    
    parser = argparse.ArgumentParser()
    parser.add_argument('--datapath', type=str, default=r"../data/New_cases_csv")
    args = parser.parse_args()
    load_data(args.datapath)