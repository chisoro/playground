import pandas as pd
import os
import csv

#f = os.getcwd()
mycsv = csv.reader(open('c:\conn\conn.csv'))
for row in mycsv:
    f = row[0]

#f = str(f.removesuffix('\\Bin'))
#f = str(f.removesuffix('\\Progs'))
#f = str(f.removesuffix('\\Reports'))
#f = str(f.removesuffix('\\work'))

fs = f + "reports\\pitem.csv"
fi = f + "reports\\pitem1.csv"
fo = f + "reports\\payrollreport.csv"
df = pd.read_csv(fs)

pivot = df.pivot(index=['DEPT','GRADE','CODE','NAME'], columns='ITEM')
pivot = pivot.fillna(0)

#pivot.to_csv('c:/simacc/reports/test.csv')
#df1 = pd.read_csv(r'C:\simacc\reports\test.csv')

#df1.to_csv('c:/simacc/reports/test_edit.csv', header = False, index = False)
      
#os.startfile(r'c:\simacc\reports\test_edit.csv')

pivot.to_csv(fi)
df1 = pd.read_csv(fi)

df1.to_csv(fo, header = False, index = False)
os.remove(fi)
  
os.startfile(fo)

