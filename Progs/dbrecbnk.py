import pandas as pd
import os
import csv
with open('z:\conn\conn.csv') as t:
    mycsv = csv.reader(t)
    for row in mycsv:
        f = row[0]
ft = f + "reports\\static.csv"
with open(ft) as st:
    mycsv1 = csv.reader(st)
    for row in mycsv1:
        fs = row[0]
        opt = int(row[1])
#fs = f + "reports\\bank11_01_2024_035746.csv"
#fi = f + "reports\\bankreport1.csv"

fo = fs.replace(r"bank","bankreport")
#give fo a dynamic name using date and time
df = pd.read_csv(fs)
df.dropna(inplace = True)
if opt == 1:
    pivot = df.pivot(index=['INCOME'], columns='CURRENCY')
    pivot = pivot.fillna(0)
    pivot.columns = ['_'.join(str(s).strip() for s in col if s) for col in pivot.columns]
    pivot.reset_index(inplace=True)
    pivot.columns = pivot.columns.str.replace(r"AMOUNT_","",regex=True)
    pivot.to_csv(fo)
else:
        pivot = df.pivot(index=['DATE','CASHIER','COLLECTION'], columns='CASHBOOK')
        pivot = pivot.fillna(0)
        pivot.columns = ['_'.join(str(s).strip() for s in col if s) for col in pivot.columns]
        pivot.reset_index(inplace=True)
        pivot.columns = pivot.columns.str.replace(r"AMOUNT_","",regex=True)
        #print(pivot)

        #pivot.to_csv(fi)
        #df1 = pd.read_csv(fi)

        #df1.to_csv(fo, header = False, index = False)
        #os.remove(fi)

        #os.startfile(fo)
        #summary = pivot.groupby("CASHIER")
        #summary_df = summary.sum()
        #summary1 = pivot.groupby("BANK")
        #summary_df1 =summary1.sum()
        summary2 = pivot.groupby("COLLECTION")
        summary_df2 = summary2.sum()
        #to drop cashier from the report
        #summary3 = pivot.groupby(["BANK", "CASHBOOK"])
        #summary_df3 = summary3.sum()
        #summary4 = pivot.groupby(["CASHIER","BANK", "CASHBOOK"])
        #summary_df4 = summary4.sum()
        tf = summary_df2.drop(['CASHIER'], axis=1)


        #df1 =pd.concat([
        #    pd.concat([summary_df],axis = 1),
        #    pd.concat([summary_df1],axis = 1),
        #    pd.concat([summary_df2],axis = 1),
        #    pd.concat([summary_df3],axis = 1),
        #    pd.concat([summary_df4],axis = 1)
        #        ]).to_csv(fo)
        df1 =pd.concat([
            pd.concat([tf],axis = 1)
                ]).to_csv(fo)
t.close()
st.close()
os.startfile(fo)
