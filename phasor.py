import os
import csv
import matplotlib.pyplot as plt

os.chdir(os.path.dirname(os.path.realpath(__file__)))

labels = {
	"A138":0,"AA138":1,"B138":2,"AB138":3,"C138":4,"AC138":5,
	"A230":6,"AA230":7,"B230":8,"AB230":9,"C230":10,"AC230":11,
	"IA1381":12,"AIA1381":13,"IB1381":14,"AIB1381":15,"IC1381":16,"AIC1381":17,
	"IA2301":18,"AIA2301":19,"IB2301":20,"AIB2301":21,"IC2301":22,"AIC2301":23,
	"IA1382":24,"AIA1382":25,"IB1382":26,"AIB1382":27,"IC1382":28,"AIC1382":29,
	"IA2302":30,"AIA2302":31,"IB2302":32,"AIB2302":33,"IC2302":34,"AIC2302":35
}

def adjustAng(ang):
    aa = ang
    while aa >= 360:
        aa -= 360 
    while aa <= -360:
        aa += 360
    if aa > 180:
        aa -= 360 
    if aa < -180:
        aa += 360 
	
    return aa
	

def readPhasor(file):
    data = []
    with open(file) as csvfile:
        csvdata = csv.reader(csvfile, delimiter=';')
        for row in csvdata:
            line = []
            for c in row[2:]:
                line.append(float(c))
            data.append(line)
    return data

def showPhasor(c1, c2, data, ang, save=False):
    res = []
    for l in data:
        if ang:
            res.append(adjustAng(l[labels[c1]]-l[labels[c2]]))
        else:
            res.append(l[labels[c1]]-l[labels[c2]])
    plt.plot(res, linestyle='none', marker='o')
    plt.title('Diferenca entre ' + c1 + ' e ' + c2)
    
    if save:
        plt.savefig(c1+c2+'.png')
    else:
        plt.show()

def readAllPhasors(dir, ini=0):
    i = ini
    files = []
    file = os.path.join(dir, str(i) + '.fasor')
    data = []

    while os.path.isfile(file):
        data += readPhasor(file)
        i += 1
        file = os.path.join(dir, str(i) + '.fasor')

    return data

file = os.path.join("ATERRAMENTO", str(0) + '.fasor')
data = readPhasor(file)
showPhasor("A138","AIA1381",data,True)

data = readAllPhasors('ATERRAMENTO', 0)
showPhasor("A138","AIA1381",data,True)

