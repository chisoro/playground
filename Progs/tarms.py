import csv
import os
import datetime
import pandas as pd
from openpyxl import load_workbook
import tkinter as tk
from tkinter import messagebox

current_dir = os.getcwd()
parent_dir = os.path.dirname(current_dir)


ddd = r'\tarms\EmployeeEarnings' + str(datetime.datetime.now()).replace(" ", "").replace(":", "").replace(".", "") + '.xlsx'


wb = load_workbook(parent_dir + r'\tarms\Tarms Template.xlsx')
sheet = wb['Sheet0']
s =  parent_dir + r'\tarms\employeeEarning.csv'
df = pd.read_csv(s, engine ='python',  dtype ={'AMOUNT1': float,'AMOUNT2': float,'AMOUNT3': float,'AMOUNT4': float,'AMOUNT5': float,'AMOUNT6': float,'AMOUNT7': float,'AMOUNT8': float,'AMOUNT9': float,'AMOUNT10': float,'AMOUNT11': float,'AMOUNT12': float,'AMOUNT13': float,'AMOUNT14': float,'AMOUNT15': float,'AMOUNT16': float,'AMOUNT17': float,'AMOUNT18': float,'AMOUNT19': float,'AMOUNT20': float,'AMOUNT21': float,'AMOUNT22': float,'AMOUNT23': float,'AMOUNT24': float,'AMOUNT25': float,'AMOUNT26': float,'AMOUNT27': float,'AMOUNT28': float,'AMOUNT29': float,'AMOUNT30': float,'AMOUNT31': float,'AMOUNT32': float,'AMOUNT33': float,'AMOUNT34': float,'AMOUNT35': float,'AMOUNT36': float,'AMOUNT37': float,'AMOUNT38': float,'AMOUNT39': float,'AMOUNT40': float,'AMOUNT41': float,'AMOUNT42': float,'AMOUNT43': float,'AMOUNT44': float,'AMOUNT45': float,'AMOUNT46': float,'AMOUNT47': float,'AMOUNT48': float}, encoding= 'unicode_escape')
# Printing A to AZ using nested loops
# Single letters 'A' to 'Z'
def show_message():
    messagebox.showinfo("Information", "Earnings File Created successfully")


for i in range(len(df)):
    #print(i,  str(df.loc[i, "Employee Name"]))
    if df.loc[i, "TIN"] != 0:
        sheet['A'+ str(i + 2)] = str(df.loc[i, "TIN"])
    sheet['B'+ str(i + 2)] = str(df.loc[i, "ID"])
    sheet['C'+ str(i + 2)] = str(df.loc[i, "Name"])
    sheet['D'+ str(i + 2)] = str(df.loc[i, "Currency"])
    j = 1
    for letter in range(ord('E'), ord('Z') + 1):
        sheet[chr(letter) + str(i + 2)] = df.loc[i, "Amount" + str(j)]
        j = j + 1
    for first in range(ord('A'), ord('Z') + 1):
        for second in range(ord('A'), ord('Z') + 1):
            if first == ord('A') and second >= ord('A'):
                letter = chr(first) + chr(second)
                sheet[letter + str(i + 2)] = df.loc[i, "Amount" + str(j)]
                j = j + 1


wb.save(parent_dir + ddd)
show_message()
