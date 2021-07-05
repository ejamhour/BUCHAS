import glob
import re
import os
import psutil
import sys, subprocess
import time
import shutil

SYNCDIR = 'C:\\Users\\ejamh\\OneDrive\\DASHBOARD-LAPTOP-511P33T3'

def copyfiles(src_dir, dst_dir):

    for filename in glob.glob(os.path.join(src_dir, '*.*')):
        shutil.copy(filename, dst_dir)

def checkLastFile(dataset):

    files = glob.glob(dataset + '\*.txt')
    files.sort(key=os.path.getmtime)
    file = files[-1]
    f = open(file)
    cisei = f.readline()
    f.close()

    n = re.search('->(.+).cisei', cisei)
    return (file, int(n.group(1)))

def checkStatus():
    status = 0
    
    ps = psutil.pids()
    for p in ps:
        try:
            if  psutil.Process(p).name().lower() == 'USB_Commands_v2.exe'.lower():
                status += 1
            if  psutil.Process(p).name().lower() == 'wolframscript.exe'.lower():
                status += 2
        except:
            print('o processo ', p, ' nao existe')
            
    return(status)

dir = os.path.dirname(os.path.realpath(__file__))
os.chdir(dir)

while(True):
    
    if checkStatus() >= 2:
        print('wolframscript is running ....')
    else:
        # subprocess.Popen(['wolframscript', os.path.join(dir, 'phasors.wls')])
        print(os.path.join(dir, 'phasors.wls'))
        subprocess.call(['wolframscript', os.path.join(dir, 'phasors.wls')])
        subprocess.call(['wolframscript', os.path.join(dir, 'dashboard.wls')])
        copyfiles(os.path.join(dir, 'DASHBOARD'), SYNCDIR)
        print('Files copied to the CLOUD DASHBOARD')
    
    time.sleep(60*10)

  











