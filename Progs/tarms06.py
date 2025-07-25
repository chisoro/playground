import csv
import os
import datetime
import pandas as pd
from openpyxl import load_workbook
import tkinter as tk
from tkinter import messagebox

current_dir = os.getcwd()
parent_dir = os.path.dirname(current_dir)


ddd = r'\tarms\EmployeeDetails' + str(datetime.datetime.now()).replace(" ", "").replace(":", "").replace(".", "") + '.xlsx'


wb = load_workbook(parent_dir + r'\tarms\employeetemplate.xlsx')
sheet = wb['Sheet1']
s =  parent_dir + r'\tarms\employeeDetails.csv'
df = pd.read_csv(s, engine ='python', encoding= 'unicode_escape')

def show_message():
    messagebox.showinfo("Information", "Employee Details File Created successfully")



for i in range(len(df)):
    #print(i,  str(df.loc[i, "Employee Name"]))
    sheet['A'+ str(i + 2)] = str(df.loc[i, "IDNo"])
    sheet['B'+ str(i + 2)] = str(df.loc[i, "FName"])
    sheet['C'+ str(i + 2)] = str(df.loc[i, "MName"])
    sheet['D'+ str(i + 2)] = str(df.loc[i, "SName"])
    sheet['E'+ str(i + 2)] = str(df.loc[i, "DOB"])
    sheet['F'+ str(i + 2)] = str(df.loc[i, "DOE"])
    sheet['G'+ str(i + 2)] = str(df.loc[i, "PDate"])
    sheet['H'+ str(i + 2)] = str(df.loc[i, "Designation"])


wb.save(parent_dir + ddd)
show_message()
