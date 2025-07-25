#Program to send mail with statement attachments to customers
#Aouther.: Simbarashe chisoro
#Date.: 25/11/2023
#The code can be reproduced with permission from Jobsim
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import landscape,A4
from reportlab.lib.pagesizes import letter
from reportlab.lib import colors
import os
import pandas as pd
import os
import csv
import smtplib
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

mycsv = csv.reader(open('c:\conn\conn.csv'))
for row in mycsv:
    f = row[0]
fs = f + "reports\\statements.csv"
ft = f + "reports\\static.csv"
df = pd.read_csv(fs)
mycsv1 = csv.reader(open(ft))
for row in mycsv1:
    CoName = row[0]
    Add1 = row[1]
    Add2 = row[2]
    Add3 = row[3]
    regn = row[4]
    phone = row[5]
    emessage = row[6]
    server = row[7]
    port = row[8]
    password = row[9]
    emailadd = row[10]

def send_email(subject, body, sender, recipients, password, server, port):
    #msg = MIMEText(body)
    msg = MIMEMultipart()
    msg['Subject'] = subject
    msg['From'] = sender
    msg['To'] = ', '.join(recipients)
    html_part = MIMEText(body)
    msg.attach(html_part)
    msg.attach(part)
    #smtp.gmail.com', 465
    with smtplib.SMTP_SSL(server, port) as smtp_server:
       smtp_server.login(sender, password)
       smtp_server.sendmail(sender, recipients, msg.as_string())
    print(pivot.loc[i, "Name"], pivot.loc[i, "Account"], "Message sent!")

pivot = df.pivot(index=['Account','Due Date','Accounting Date','Period','Name','Address1','Address2','Address3','Stand Number','Street','Email','Cell','Message1','Message2'], columns='ITEM')

pivot.columns = ['_'.join(str(s).strip() for s in col if s) for col in pivot.columns]
pivot.reset_index(inplace=True)
pivot.columns = pivot.columns.str.replace(r"AMOUNT_","",regex=True)
print(pivot)
#loop through the data frame creating pdf files and sending them by email
for i in range(len(pivot)):
    #print(pivot.loc[i, "Name"], pivot.loc[i, "Account"])
    fn = f + "reports\\" + str(pivot.loc[i, "Account"]) + ".pdf"
    def create_pdf():
        c = canvas.Canvas(fn, pagesize=A4)
        #canvas.Canvas.setPageSize(c, (landscape(A4)))

        image_path = os.path.join(os.getcwd(), "logo.jpg")
        c.drawImage(image_path, 50, 725, width=100, height=100)

        c.drawString(350,800,str(CoName))
        if(Add1 != 'nan'):
            c.drawString(350,785,str(Add1))
        if(Add2 != 'nan'):
            c.drawString(350,770,str(Add2))
        if(Add3 != 'nan'):
            c.drawString(350,755,str(Add3))
        if(phone != 'nan'):
            c.drawString(350,740,"PHONE: " + str(phone))
        if(regn != 'nan'):
            c.drawString(350,725,"REG. NUMBER: " + str(regn))

        c.setFillColor(colors.red)
        c.setFont("Helvetica-Bold", 24)
        c.drawString(50, 680, "Account Statement")

        c.setFillColor(colors.black)
        c.setFont("Helvetica", 14)
        c.drawString(50, 660, str(pivot.loc[i, "Name"]))
        if(str(pivot.loc[i, "Address1"]) != 'nan'):
            c.drawString(50, 645, str(pivot.loc[i, "Address1"]))
        if(str(pivot.loc[i, "Address2"]) != 'nan'):
            c.drawString(50, 630, str(pivot.loc[i, "Address2"]))
        if(str(pivot.loc[i, "Address3"]) != 'nan'):
            c.drawString(50, 615, str(pivot.loc[i, "Address3"]))
        if(str(pivot.loc[i, "Cell"]) != 'nan'):
            c.drawString(50, 600, str(pivot.loc[i, "Cell"]) + "/"+ str(pivot.loc[i, "Email"]))
        c.drawString(350,660,"Account No.:")
        c.drawString(450, 660, str(pivot.loc[i, "Account"]))
        c.drawString(350,645,"Acc Date.:")
        c.drawString(450, 645, str(pivot.loc[i, "Accounting Date"]))
        c.drawString(350,630,"Due Date.:")
        c.drawString(450, 630, str(pivot.loc[i, "Due Date"]))
        c.drawString(350,615,"Period.:")
        c.drawString(450, 615, str(pivot.loc[i, "Period"]))
        c.drawString(50, 575, "===============================================================")
        c.drawString(50, 560, "Date             Ref             Description                                                    Amount")
        c.drawString(50, 545, "                                       Balance BF")
        cname = "Balance BF"

        vamt = "{:.2f}".format(pivot.loc[i,cname])
        #print('{:>10.2f}'.format(vamt),sep='')
        #print(vamt)
        vlen = 12 + (4 - (len(str(vamt))))
        #print(vlen)
        vamt = vamt.rjust(vlen)
        c.drawString(475, 545, vamt)
        j = 545
        for k  in range(14, pivot.shape[1]):
            cname = pivot.columns[k]
            if ((cname != "90-days+") and (cname != "60-Days") and (cname != "Balance BF")  and (cname != "30-Days") and (cname != "Current") and (cname != "Balance CF") and (cname != "Credit")):
                vamt = "{:.2f}".format(pivot.loc[i,cname])

                #print(vamt)
                if (vamt != 'nan'):
                    vlen = 12 + (4 - (len(str(vamt))))
                    vamt = vamt.rjust(vlen)
                    c.drawString(50, j - 15, cname)
                    c.drawString(475, j - 15, vamt)
                    j = j - 15

        c.drawString(50, 140, "===============================================================")
        j = j - 15
        c.drawString(50, 125, "CREDIT")
        c.drawString(135,125, "90-Days+")
        c.drawString(220, 125, "60-Days")
        c.drawString(305, 125, "30-Days")
        c.drawString(390, 125, "Current")
        c.drawString(475, 125,"Amount Due")
        j = j -15
        credit = "{:.2f}".format(pivot.loc[i,"Credit"])
        age_1 =  "{:.2f}".format(pivot.loc[i,"Current"])
        age_2 =  "{:.2f}".format(pivot.loc[i,"30-Days"])
        age_3 =  "{:.2f}".format(pivot.loc[i,"60-Days"])
        age_4 =  "{:.2f}".format(pivot.loc[i,"90-days+"])
        c.drawString(50, 110, str(credit))
        c.drawString(135, 110, str(age_4))
        c.drawString(220, 110, str(age_3))
        c.drawString(305, 110, str(age_2))
        c.drawString(390, 110, str(age_1))
        cname = "Balance CF"
        vamt = "{:>10.2f}".format(pivot.loc[i,cname])
        c.drawString(475, 110, str(vamt))
        if (str(pivot.loc[i,"Message1"]) != 'nan'):
            c.drawString(50, 80, str(pivot.loc[i,"Message1"]))
        if (str(pivot.loc[i,"Message2"]) != 'nan'):
            c.drawString(50, 65, str(pivot.loc[i,"Message2"]))
        c.drawString(270, 50, "Thank You")
        c.save()

    if __name__ == "__main__":
        create_pdf()

    subject = str(pivot.loc[i, "Account"]) + " Account Statement for period.: " + str(pivot.loc[i, "Period"])
    if (emessage == 'nan'):
        emessage = ""
    body = emessage
    #sender = "s.chisoroi@gmail.com"
    #sender = "systemsadmin@vfm.co.zw"
    sender = emailadd
    #recipients = ["s.chisoroi@gmail.com"]
    recipients = [str(pivot.loc[i, "Email"])]
    #password = "tavh qwhh upzd vuef"
    #password = "Vfm5331&"

    fm = str(pivot.loc[i, "Account"]) + ".pdf"
    fl = f + "reports\\"  + fm

    with open(fl, "rb") as attachment:
        # Add the attachment to the message
        part = MIMEBase("application", "octet-stream")
        part.set_payload(attachment.read())
    encoders.encode_base64(part)
    part.add_header(
        "Content-Disposition",
        f"attachment; filename = " + fm,
    )
    print(pivot.loc[i, "Name"], pivot.loc[i, "Account"])
    #send_email(subject, body, sender, recipients, password, server, port)
    #delete file
    #os.remove(fl)
#remove all working files.
#os.remove(fs)
#os.remove(ft)
print("Finished")
