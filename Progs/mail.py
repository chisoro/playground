import smtplib
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

subject = "Testing python"
body = "This is the body of the text message"
#sender = "s.chisoroi@gmail.com"
sender = "systemsadmin@vfm.co.zw"
recipients = ["s.chisoroi@gmail.com"]
#password = "tavh qwhh upzd vuef"
password = "Vfm5331&"
fn = "content_pdf.pdf"
fl = "c:\\simacc\\Progs\\" + fn

with open(fl, "rb") as attachment:
    # Add the attachment to the message
    part = MIMEBase("application", "octet-stream")
    part.set_payload(attachment.read())
encoders.encode_base64(part)
part.add_header(
    "Content-Disposition",
    f"attachment; filename = " + fn,
)



def send_email(subject, body, sender, recipients, password):
    #msg = MIMEText(body)
    msg = MIMEMultipart()
    msg['Subject'] = subject
    msg['From'] = sender
    msg['To'] = ', '.join(recipients)
    html_part = MIMEText(body)
    msg.attach(html_part)
    msg.attach(part)
    #smtp.gmail.com', 465
    with smtplib.SMTP_SSL('smtp.liquidmail.tech', 465) as smtp_server:
       smtp_server.login(sender, password)
       smtp_server.sendmail(sender, recipients, msg.as_string())
    print("Message sent!")


send_email(subject, body, sender, recipients, password)
