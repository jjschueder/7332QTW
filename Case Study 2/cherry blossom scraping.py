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


womenURLs = ["1999/cb99f.html",
            "2000/Cb003f.htm",
"2001/oof_f.html",
"2002/ooff.htm",

"2003/CB03-F.HTM",
"2004/women.htm",
"2005/womennet.htm",
"2006/womennet.htm",
"2007/women.htm",
"2008/women.htm",
"2009/09cucb-F.htm",
"2010/2010cucb10m-f.htm",
"2011/2011cucb10m-f.htm",
"2012/2012cucb10m-f.htm"]


def get_sp1(link):
        user_agent ='Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.7) Gecko/2009021910 Firefox/3.0.7'
        headers = {'User-Agent': user_agent, }
        request = urllib.request.Request(link, None, headers)  # The assembled request
        response = urllib.request.urlopen(request)
        a = response.read().decode('utf-8','ignore')
        sp = soup(a,'html.parser')
        return sp

cururl = 'http://www.cherryblossom.org/results/1999/cb99f.html'
i = 0
for i, val in enumerate(womenURLs):
    df = []
    print(womenURLs[i])
    cururl = (ubase + womenURLs[i])    
    result = get_sp1(cururl)   
#    result = get_sp1('http://www.cherryblossom.org/results/2000/Cb003f.htm')
    
    table = result.find("pre").find(text=True)
    lines = table.splitlines()
    df = pd.DataFrame(lines)
    df['year'] = womenURLs[i][0:4]

    
    if  womenURLs[i] =='1999/cb99f.html':   
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
    if  womenURLs[i] == "2000/Cb003f.htm":
        df['place'] = df[0].str[0:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:21]
        df['name'] = df[0].str[22:43]
        df['age'] = df[0].str[44:46]
        df['hometown'] = df[0].str[47:65]
        df['guntime'] = df[0].str[66:73]
        df['netTime'] = df[0].str[76:82]   
    if  womenURLs[i] =="2001/oof_f.html":
        df['place'] = df[0].str[1:5]
        df['numId'] = df[0].str[6:11]
        df['name'] = df[0].str[12:33]
        df['age'] = df[0].str[34:36]
        df['hometown'] = df[0].str[37:55]
        df['guntime'] = df[0].str[56:63]
        df['netTime'] = df[0].str[64:71]
    if  womenURLs[i] =="2002/ooff.htm":
        df['place'] = df[0].str[1:5]
        df['numId'] = df[0].str[6:11]
        df['name'] = df[0].str[12:33]
        df['age'] = df[0].str[34:36]
        df['hometown'] = df[0].str[37:55]
        df['guntime'] = df[0].str[64:72]
        df['netTime'] = df[0].str[56:63]
    if  womenURLs[i] =="2003/CB03-F.HTM":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:21]
        df['name'] = df[0].str[22:53]
        df['age'] = df[0].str[53:55]
        df['hometown'] = df[0].str[56:75]
        df['guntime'] = df[0].str[76:83]
        df['netTime'] = df[0].str[85:92]
    if  womenURLs[i] =="2004/women.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:21]
        df['name'] = df[0].str[21:52]
        df['age'] = df[0].str[52:54]
        df['hometown'] = df[0].str[55:74]
        df['guntime'] = df[0].str[83:90]
        df['netTime'] = df[0].str[75:82]        
    if  womenURLs[i] =="2005/womennet.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:22]
        df['name'] = df[0].str[22:45]
        df['age'] = df[0].str[44:48]
        df['hometown'] = df[0].str[49:67]
        df['guntime'] = df[0].str[67:75]
        df['netTime'] = df[0].str[75:83]        
    if  womenURLs[i] =="2006/womennet.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:15]
        df['numId'] = df[0].str[16:22]
        df['name'] = df[0].str[22:45]
        df['age'] = df[0].str[45:48]
        df['hometown'] = df[0].str[49:64]
        df['guntime'] = df[0].str[64:72]
        df['netTime'] = df[0].str[72:80]
        df['pace'] = df[0].str[80:87]       
    if  womenURLs[i] =="2007/women.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:70]
        df['guntime'] = df[0].str[70:77]
        #netTime'] = df[0].str[73:80]
        df['pace'] = df[0].str[79:84]        
    if  womenURLs[i] =="2008/women.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:70]
        df['guntime'] = df[0].str[98:106]
        #netTime'] = df[0].str[73:80]
        df['pace'] = df[0].str[107:112] 
    if  womenURLs[i] =="2009/09cucb-F.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[72:79]
        df['netTime'] = df[0].str[80:87]
        df['pace'] = df[0].str[89:94]      
    if  womenURLs[i] =="2010/2010cucb10m-f.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[80:87]
        df['netTime'] = df[0].str[88:95]
        df['pace'] = df[0].str[97:102]  
    if  womenURLs[i] =="2011/2011cucb10m-f.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[80:87]
        df['netTime'] = df[0].str[88:95]
        df['pace'] = df[0].str[97:102]
    if  womenURLs[i] =="2012/2012cucb10m-f.htm":
        df['place'] = df[0].str[1:5]
        df['div_total'] = df[0].str[6:17]
        df['numId'] = df[0].str[18:24]
        df['name'] = df[0].str[25:48]
        df['age'] = df[0].str[48:50]
        df['hometown'] = df[0].str[51:72]
        df['guntime'] = df[0].str[80:87]
        # netTime'] = df[0].str[89:95]
        df['pace'] = df[0].str[89:94]

    dfbig = dfbig.append(df)
    
dfbig['place'] = dfbig.place.str.strip()    
dfbig['guntime'] = dfbig.guntime.str.strip()
dfbig['netTime'] = dfbig.netTime.str.strip()
dfbig['pace'] = dfbig.pace.str.strip()
dfbig['age'] = dfbig.age.str.strip()
dfbig['time'] = dfbig.time.str.strip()
dfbig = dfbig[dfbig.place.apply(lambda x: x.isnumeric())]     
dfbig ['combtime'] = dfbig['time'].fillna(dfbig['guntime'])
import numpy as np
dfbig['combtime'] = dfbig ['combtime'].replace('', np.nan)
dfbig = dfbig.dropna(subset=['combtime'])

dfbig.describe()
dfbig.to_csv("C:/Users/jjschued/Documents/SMU/7333 QTW/womensrace.csv")
