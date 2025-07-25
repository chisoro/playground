from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import landscape,A4
from reportlab.lib.pagesizes import letter
from reportlab.lib import colors
import os
import pandas as pd
import os
import csv

mycsv = csv.reader(open('c:\conn\conn.csv'))
for row in mycsv:
    f = row[0]
fs = f + "reports\\statements.csv"
df = pd.read_csv(fs)
def create_pdf():
    c = canvas.Canvas("content_pdf.pdf", pagesize=A4)
    #canvas.Canvas.setPageSize(c, (landscape(A4)))

    image_path = os.path.join(os.getcwd(), "logo.jpg")
    c.drawImage(image_path, 50, 700, width=150, height=150)

    c.setFillColor(colors.red)
    c.setFont("Helvetica-Bold", 24)
    c.drawString(50, 680, "Creating PDFs with Python")

    c.setFillColor(colors.black)
    c.setFont("Helvetica", 14)
    c.drawString(50, 660, "In this tutorial, we will demonstrate how to create PDF files using Python.")
    c.drawString(50, 640, "Python is a versatile programming language that can be used to create different types of files, including PDFs.")
    c.drawString(50, 620, "By the end of this tutorial, you will be able to generate PDF files using Python and the ReportLab library.")
    c.save()

if __name__ == "__main__":
    create_pdf()
