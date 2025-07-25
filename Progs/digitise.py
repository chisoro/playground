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
import re
import logging
import sys

# Create a custom logger
logger = logging.getLogger('StatementMailer')
logger.setLevel(logging.DEBUG)  # Log all levels from DEBUG and above

# Create handlers
file_handler = logging.FileHandler('application.log')
file_handler.setLevel(logging.DEBUG)

console_handler = logging.StreamHandler(sys.stdout)
console_handler.setLevel(logging.DEBUG)

# Create a formatter and set it for both handlers
formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s',
                              datefmt='%Y-%m-%d %H:%M:%S')
file_handler.setFormatter(formatter)
console_handler.setFormatter(formatter)

# Add handlers to the logger (avoid duplicate handlers if you rerun this)
if not logger.hasHandlers():
    logger.addHandler(file_handler)
    logger.addHandler(console_handler)
else:
    logger.handlers.clear()
    logger.addHandler(file_handler)
    logger.addHandler(console_handler)

current_dir = os.getcwd()
parent_dir = os.path.dirname(current_dir)



#parent_dir = os.path.dirname(parent_dir)
try:
        mycsv = csv.reader(open(parent_dir + '\\conn\\conn.csv'))

        for row in mycsv:
            f = row[0]
        fs = f + "reports\\statements.csv"
        ft = f + "reports\\static.csv"
        fz = f + "reports\\emaillog.csv"
        #all transactions
        df1 = pd.read_csv(fs, engine ='python',  dtype ={'AMOUNT': float}, encoding= 'unicode_escape')
        #only those with emails but not validated
        df = df1[df1['Email'].notna() & (df1['Email'].str.strip() != '')]

        #pivot used create all statements
        pivot = df1.pivot(index=['Account','Due Date','Accounting Date','Period','Name','Address1','Address2','Address3','Stand Number','Street','Email','Cell','Message1','Message2'], columns='ITEM')
        pivot.columns = ['_'.join(str(s).strip() for s in col if s) for col in pivot.columns]
        pivot.reset_index(inplace=True)
        pivot.columns = pivot.columns.str.replace(r"AMOUNT_","",regex=True)

        #pivot to send only valid emails
        pivot1 = df.pivot(index=['Account','Due Date','Accounting Date','Period','Name','Address1','Address2','Address3','Stand Number','Street','Email','Cell','Message1','Message2'], columns='ITEM')
        pivot1.columns = ['_'.join(str(s).strip() for s in col if s) for col in pivot1.columns]
        pivot1.reset_index(inplace=True)
        pivot1.columns = pivot1.columns.str.replace(r"AMOUNT_","",regex=True)

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
            bpn = row[11]
            elmail =row[12]
except Exception as e:
        logger.error(f"Error: {e}")



def validate_email(email):
    if re.match(r"[^@]+@[^@]+\.[^@]+", email):
        return True
    return False

def send_email(subject, body, sender, recipients, password, server, port, elmail, attachment_part=None):
    msg = MIMEMultipart()
    msg['Subject'] = subject
    msg['From'] = sender
    REPLY_TO_ADDRESS = elmail  # make sure elmail is accessible or pass as a parameter
    msg['To'] = ', '.join(recipients)
    msg.add_header('reply-to', REPLY_TO_ADDRESS)

    html_part = MIMEText(body)
    msg.attach(html_part)

    if attachment_part is not None:
        msg.attach(attachment_part)

    try:
        with smtplib.SMTP_SSL(server, port) as smtp_server:
            smtp_server.login(sender, password)
            smtp_server.sendmail(sender, recipients, msg.as_string())
    except Exception as e:
        logger.error(f"Failed to send email to {recipients} with subject '{subject}'. Error: {e}")




def create_pdf():
    c = canvas.Canvas(fn, pagesize=A4)
    #canvas.Canvas.setPageSize(c, (landscape(A4)))

    #image_path = os.path.join(os.getcwd(), "logo.jpg")
    #fp = os.path.join(f,\\progs\sedmail\\logo.jpg)
    #print(fp)
    c.setFillColor(colors.red)
    c.setFont("Helvetica-Bold", 24)
    c.drawString(50, 800, "Account Statement")

    image_path = f + "bin\\sendmail\\logo.jpg"
    c.drawImage(image_path, 50, 685, width=100, height=100)

    c.setFillColor(colors.black)
    c.setFont("Helvetica", 14)

    c.drawString(350,785,str(CoName))
    if(Add1 != 'nan'):
        c.drawString(350,770,str(Add1))
    if(Add2 != 'nan'):
        c.drawString(350,755,str(Add2))
    if(Add3 != 'nan'):
        c.drawString(350,740,str(Add3))
    c.setFont("Helvetica", 12)
    if(phone != 'nan') and (phone != 0.0):
        c.drawString(350,725,"PHONE: " + str(phone))
    c.setFont("Helvetica", 14)
    if(regn != 'nan'):
        c.drawString(350,710,"VAT NUMBER: " + str(regn))
    if(bpn != 'nan'):
        c.drawString(350,695,"TIN: " + str(bpn))
    if(elmail != 'nan'):
        c.drawString(350,680,"EMAIL: " + str(elmail))
    #c.setFillColor(colors.red)
    #c.setFont("Helvetica-Bold", 24)
    #c.drawString(50, 680, "Account Statement")

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
        phonen = str(pivot.loc[i, "Cell"])
        separator = '.'
        phonen = phonen.split(separator, 1)[0]
        c.drawString(50, 600, phonen + "/"+ str(pivot.loc[i, "Email"]))
        #c.drawString(50, 600, str(pivot.loc[i, "Cell"]) + "/"+ str(pivot.loc[i, "Email"]))
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
    logger.info(f"PDF created: {fn} for account {pivot.loc[i, 'Account']} - {pivot.loc[i, 'Name']}")


def s_mail(pivot1, emessage):
    for i in range(len(pivot1)):
        subject = f"{pivot1.loc[i, 'Account']} Account Statement for period.: {pivot1.loc[i, 'Period']}"
        body = "" if emessage == 'nan' else emessage
        sender = emailadd
        recipients = [str(pivot1.loc[i, "Email"])]
        fm = f"{pivot1.loc[i, 'Account']}.pdf"
        fl = os.path.join(f, "reports\\statements", fm)  # safer path join

        try:
            with open(fl, "rb") as attachment:
                part = MIMEBase("application", "octet-stream")
                part.set_payload(attachment.read())
            encoders.encode_base64(part)
            part.add_header(
                "Content-Disposition",
                f"attachment; filename={fm}",
            )
        except FileNotFoundError:
            logger.error(f"Attachment file not found: {fl} for account {pivot1.loc[i, 'Account']}")
            continue

        if validate_email(str(pivot1.loc[i, "Email"])):
            send_email(subject, body, sender, recipients, password, server, port, elmail, attachment_part=part)
            logger.info(f"Email sent to {recipients} with subject '{subject}'")
        else:
            logger.warning(f"Message not sent! Invalid email address for account {pivot1.loc[i, 'Account']} - {pivot1.loc[i, 'Name']}")



for i in range(len(pivot)):
    #print(pivot.loc[i, "Name"], pivot.loc[i, "Account"])
    fn = f + "reports\\statements\\" + str(pivot.loc[i, "Account"]) + ".pdf"
    if __name__ == "__main__":
        create_pdf()

s_mail(pivot1, emessage)
