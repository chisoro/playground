import tkinter
from tkinter import filedialog
#import tkFileDialog
import os
import csv
import pandas as pd
root = tkinter.Tk()
root.withdraw() #use to hide tkinter window

currdir = os.getcwd()
#tempdir = filedialog.askdirectory(parent=root, initialdir=currdir, title='Please select a directory')
tempfile = filedialog.askopenfilename(parent=root, initialdir=currdir, title='Please select file')
#if len(tempdir) > 0:
    #print("You chose %s" % tempdir)
#if len(tempfile) >  0:
#    print("You chose %s" % tempfile)
#seed_name = ""
fo = "c:\\simacc\\reports\\germplasm.csv"
df = pd.read_csv(tempfile)

seed_name = ""
for i in range(len(df)):
    seed_name = seed_name + "'"+str(df.loc[i, "DESIGNATION"])+"',"

seed_name = seed_name.rstrip(seed_name[-1])
print(seed_name)
