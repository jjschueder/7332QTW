#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May 25 11:03:07 2020

@author: jjschued
"""

import datetime
#from func_timeout import func_set_timeout, FunctionTimedOut
from bs4 import BeautifulSoup as soup
import requests
import urllib3
import urllib
import urllib.parse
from datetime import datetime
import json
import configparser
import random
import pandas as pd
# # URLs ------------------------------------------------------
ubase = "http://www.cherryblossom.org/results/"
#years = 1999:2012


menURLs = ["1999/cb99m.html",
            "2000/Cb003m.htm",
"2001/oof_m.html",
"2002/oofm.htm",
"2003/CB03-M.HTM",
"2004/men.htm",
"2005/mennet.htm",
"2006/mennet.htm",
"2007/men.htm",
"2008/men.htm",
"2009/09cucb-M.htm",
"2010/2010cucb10m-m.htm",
"2011/2011cucb10m-m.htm",
"2012/2012cucb10m-m.htm"]


def get_sp1(link, year):
        user_agent ='Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.7) Gecko/2009021910 Firefox/3.0.7'
        headers = {'User-Agent': user_agent, }
        request = urllib.request.Request(link, None, headers)  # The assembled request
        response = urllib.request.urlopen(request)
        a = response.read().decode('utf-8','ignore')
        if link != 'http://www.cherryblossom.org/results/2000/Cb003m.htm':
            sp = soup(a,'html.parser')
            return sp
        elif link == 'http://www.cherryblossom.org/results/2000/Cb003m.htm':
            index=a.find("PLACE")
            ab = a[index:]
            index=ab.find("# Under")
            ab = ab[:index]
            return ab

# =============================================================================
# i=1
# df = []
# print(menURLs[i])
# cururl = (ubase + menURLs[i])    
# result = get_sp1(cururl, menURLs[i])   
# if menURLs[i] != '2000/Cb003m.htm':
#     table = result.find("pre").find(text=True)
# elif menURLs[i] == '2000/Cb003m.htm':
#     table = result
# lines = table.splitlines()
# df = pd.DataFrame(lines)
# df['year'] = menURLs[i][0:4]
# print(df)
#               
# =============================================================================
#cururl = 'http://www.cherryblossom.org/results/2003/CB03-M.HTM' 
#link = cururl
#link = 'http://www.cherryblossom.org/results/1999/cb99f.html' 
i = 0
dfbig = []
for i, val in enumerate(menURLs):
    df = []
    print(menURLs[i])
    cururl = (ubase + menURLs[i])    
    result = get_sp1(cururl, menURLs[i])   
    if menURLs[i] != '2000/Cb003m.htm':
        table = result.find("pre").find(text=True)
    elif menURLs[i] == '2000/Cb003m.htm':
        table = result
    lines = table.splitlines()
    df = pd.DataFrame(lines)
    df['year'] = menURLs[i][0:4]

    
    if  menURLs[i] =='1999/cb99m.html':   
 #       rownumber = df.index[df[0].str[:2] == '=='].tolist()
 #       print(rownumber)
 #       rownumber = rownumber[0] + 1
 #       df = df.loc[rownumber:len(df)]
        df['place'] = df[0].str[0:5]
        df['div_total'] = df[0].str[6:15]
        df['name'] = df[0].str[16:37]
        df['age'] = df[0].str[38:40]
        df['hometown'] = df[0].str[41:59]
        df['time'] = df[0].str[60:67]
        df['pace'] = df[0].str[68:73]
        dfbig = df.copy()
    if  menURLs[i] == "2000/Cb003m.htm":
        df['place'] = df[0].str[0:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:21]
        df['name'] = df[0].str[22:43]
        df['age'] = df[0].str[44:46]
        df['hometown'] = df[0].str[47:65]
        df['guntime'] = df[0].str[66:73]
        df['netTime'] = df[0].str[75:82]   
    if  menURLs[i] =="2001/oof_m.html":
        df['place'] = df[0].str[1:5]
        df['numId'] = df[0].str[6:11]
        df['name'] = df[0].str[12:33]
        df['age'] = df[0].str[34:36]
        df['hometown'] = df[0].str[37:55]
        df['guntime'] = df[0].str[56:63]
        df['netTime'] = df[0].str[64:71]
    if  menURLs[i] =="2002/oofm.htm":
        df['place'] = df[0].str[1:5]
        df['numId'] = df[0].str[6:11]
        df['name'] = df[0].str[12:33]
        df['age'] = df[0].str[34:36]
        df['hometown'] = df[0].str[37:55]
        df['guntime'] = df[0].str[64:72]
        df['netTime'] = df[0].str[56:63]
    if  menURLs[i] =="2003/CB03-M.HTM":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:21]
        df['name'] = df[0].str[22:53]
        df['age'] = df[0].str[53:55]
        df['hometown'] = df[0].str[56:75]
        df['guntime'] = df[0].str[76:83]
        df['netTime'] = df[0].str[85:92]
    if  menURLs[i] =="2004/men.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:21]
        df['name'] = df[0].str[21:52]
        df['age'] = df[0].str[52:54]
        df['hometown'] = df[0].str[55:74]
        df['guntime'] = df[0].str[83:90]
        df['netTime'] = df[0].str[75:82]        
    if  menURLs[i] =="2005/mennet.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:22]
        df['name'] = df[0].str[22:45]
        df['age'] = df[0].str[44:48]
        df['hometown'] = df[0].str[48:67]
        df['guntime'] = df[0].str[67:75]
        df['netTime'] = df[0].str[75:83]        
    if  menURLs[i] =="2006/mennet.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:22]
        df['name'] = df[0].str[22:45]
        df['age'] = df[0].str[45:48]
        df['hometown'] = df[0].str[48:64]
        df['guntime'] = df[0].str[64:72]
        df['netTime'] = df[0].str[72:80]
        df['pace'] = df[0].str[80:87]       
    if  menURLs[i] =="2007/men.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:70]
        df['guntime'] = df[0].str[70:77]
        #netTime'] = df[0].str[73:80]
        df['pace'] = df[0].str[79:84]        
    if  menURLs[i] =="2008/men.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:70]
        df['guntime'] = df[0].str[98:106]
        #netTime'] = df[0].str[73:80]
        df['pace'] = df[0].str[106:112] 
    if  menURLs[i] =="2009/09cucb-M.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[72:79]
        df['netTime'] = df[0].str[80:87]
        df['pace'] = df[0].str[89:94]      
    if  menURLs[i] =="2010/2010cucb10m-m.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[80:87]
        df['netTime'] = df[0].str[88:95]
        df['pace'] = df[0].str[97:102]  
    if  menURLs[i] =="2011/2011cucb10m-m.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[80:87]
        df['netTime'] = df[0].str[88:95]
        df['pace'] = df[0].str[96:102]
    if  menURLs[i] =="2012/2012cucb10m-m.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[80:87]
        # netTime'] = df[0].str[89:95]
        df['pace'] = df[0].str[87:94]
    if  menURLs[i] !='1999/cb99m.html':
        dfbig = dfbig.append(df)

    
dfbig['place'] = dfbig.place.str.strip()    
dfbig['guntime'] = dfbig.guntime.str.strip()
dfbig['netTime'] = dfbig.netTime.str.strip()
dfbig['pace'] = dfbig.pace.str.strip()
dfbig['age'] = dfbig.age.str.strip()
dfbig['time'] = dfbig.time.str.strip()
dfbig = dfbig[dfbig.place.apply(lambda x: x.isnumeric())]     
dfbig ['combtime'] = dfbig['time'].fillna(dfbig['netTime'])
dfbig ['combtime'] = dfbig['combtime'].fillna(dfbig['guntime'])
import numpy as np
dfbig['combtime'] = dfbig ['combtime'].replace('', np.nan)
dfbig['combtime'] = dfbig ['combtime'].str.replace('#', '')
dfbig['combtime'] = dfbig ['combtime'].str.replace('*', '')
dfbig = dfbig.dropna(subset=['combtime'])


dfbig.describe()
dfbig.to_csv("C:/Users/jjschued/Documents/SMU/7333 QTW/mensrace.csv")
