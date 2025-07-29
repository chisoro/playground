# Author: Colleen Marasha
import json
import re
from datetime import datetime
from hashlib import md5
import base64
import qrcode
from reportlab.pdfgen import canvas
from reportlab.lib.units import mm
import os
import textwrap

# Set up default positions
x_pos = 2 * mm               # left margin
right_x = 75 * mm            # right margin
line_height = 12             # space between lines


# === Load JSON Data ===
def load_clean_json(path):
    with open(path, "r") as f:
        raw = f.read()
    cleaned = re.sub(r",\s*([}\]])", r"\1", raw)
    return json.loads(cleaned)

receipt = load_clean_json("receipt.json")["receipt"]
with open("recresponse.json") as f:
    rec_response = json.load(f)

# === Generate QR Code Hash ===
qr_url = rec_response.get("qrUrl", "https://receipt.zimra.org/")

device_id = str(receipt.get("deviceID") or receipt.get("creditDebitNote", {}).get("deviceID", 0)).zfill(10)
receipt_date = datetime.strptime(receipt["receiptDate"], "%Y-%m-%dT%H:%M:%S").strftime("%d%m%Y")
receipt_global_no = str(receipt["receiptGlobalNo"]).zfill(10)
sig_hash = rec_response.get("receiptDeviceSignature", {}).get("hash") or rec_response.get("receiptServerSignature", {}).get("hash")
raw_bytes = base64.b64decode(sig_hash)
qr_data = md5(raw_bytes).hexdigest().upper()[:16]
qr_string = f"{qr_url}{device_id}{receipt_date}{receipt_global_no}{qr_data}"

# === Generate QR Code Image ===
qr_img = qrcode.make(qr_string)
qr_path = "qr_receipt48.png"
qr_img.save(qr_path)

# === Create Receipt PDF ===
receipt_width = 80 * mm
receipt_height = 300 * mm
pdf_path = "receipt48.pdf"
c = canvas.Canvas(pdf_path, pagesize=(receipt_width, receipt_height))
c.setFont("Courier", 8)

x = 5
y = receipt_height - 10

def line(txt="", offset=12, font="Courier", size=8):
    global y
    c.setFont(font, size)
    c.drawString(x_pos, y, txt)
    y -= offset

def hyperlink(text, url, offset=12):
    global y
    c.setFillColorRGB(0, 0, 1)  # blue
    c.drawString(x, y, text)
    c.linkURL(url, (x, y - 1, x + 150, y + 10), relative=1)
    c.setFillColorRGB(0, 0, 0)  # reset to black
    y -= offset


def hyperlink_centered(text, url, font="Courier", size=8):
    global y
    c.setFont(font, size)
    text_width = c.stringWidth(text, font, size)
    c.setFillColorRGB(0, 0, 1)
    c.drawCentredString(center_x, y, text)
    c.linkURL(url, (center_x - text_width / 2, y - 2, center_x + text_width / 2, y + 8), relative=0)
    c.setFillColorRGB(0, 0, 0)
    y -= 12

bold_line = "-" * 48

# === Optional Logo ===
if os.path.exists("logo.png"):
    c.drawImage("logo.png", (receipt_width - 40) / 2, y - 40, width=40, height=40)
    y -= 50
center_offset = 15  # Space after centered text


# === Seller Info ===
seller = receipt.get("sellerData", {})
receipt_width = 80 * mm
receipt_height = 300 * mm
margin = 30
center_x = receipt_width / 2

taxpayer_name = seller.get("taxpayerName", "")
tin = f"TIN: {seller.get('taxpayerTIN', '')}"
vat_no = f"VAT No: {seller.get('vatNumber', '')}"

# Draw the centered strings under company logo
c.drawCentredString(center_x, receipt_height - 60, taxpayer_name)
c.drawCentredString(center_x, receipt_height - 75, tin)
c.drawCentredString(center_x, receipt_height - 90, vat_no)
y -= (center_offset + 30)

line(seller.get("deviceBranchName", ""))
line(seller.get("deviceBranchAddress", ""))
email = seller.get("deviceBranchContacts", {}).get("email", "")
if email:
    hyperlink(email, f"mailto:{email}")
else:
    line("")
line(seller.get("deviceBranchContacts", {}).get("phoneNo", ""))
line(bold_line)

# === FISCAL TAX INVOICE TITLE ===
y -= 5
c.setFont("Courier-Bold", 14)
c.drawCentredString(receipt_width / 2, y, "FISCAL TAX INVOICE")
y -= 15
c.setFont("Courier", 8)
line(bold_line)


# === Buyer Info ===
buyer = receipt.get("buyerData", {})

c.setFont("Courier-Bold", 8)
c.drawCentredString(center_x, y, "BUYER:")
y -= 12
c.setFont("Courier", 8)
for field in [
    buyer.get("buyerRegisterName", ""),
    buyer.get("buyerTradeName", ""),
    buyer.get("buyerAddress", {}).get("street", ""),
    buyer.get("buyerAddress", {}).get("city", ""),
    buyer.get("buyerContacts", {}).get("phoneNo", "")
]:
    if field:
        c.drawCentredString(center_x, y, field)
        y -= 12

buyer_email = buyer.get("buyerContacts", {}).get("email", "")
if buyer_email:
    hyperlink_centered(buyer_email, f"mailto:{buyer_email}")
else:
    line("")
#line(buyer.get("buyerContacts", {}).get("phoneNo", ""))
line(bold_line)

# === Receipt Metadata ===
line(f"Invoice No: {receipt['receiptCounter']}")
line(f"Fiscal Day No: {receipt['fiscalDayNo']}")
line(f"Ref No: {receipt['invoiceNo']}")
line(f"Device Serial No: {receipt['deviceSerialNo']}")
line(f"Device ID: {receipt['deviceID']}")
line(f"Date: {datetime.strptime(receipt['receiptDate'], '%Y-%m-%dT%H:%M:%S').strftime('%d/%m/%y %H:%M')}")
line(bold_line)

# === Line Items ===
line("{:<30}{:>15}".format("Description", "Amount"))
line(bold_line)
for item in receipt["receiptLines"]:
    name = item.get("receiptLineName", "")
    #qty = item.get("receiptLineQuantity", 1)
    #unit_price = item.get("receiptLinePrice", 0.00)
    amount = f"{item.get('receiptLineTotal', 0):,.2f}"

    # Wrap name to lines of max 30 characters
    name_lines = textwrap.wrap(name, width=30)

    # Print first line with name and amount
    line("{:<30}{:>15}".format(name_lines[0], amount))

    # Print the remaining lines (just the name, no amount)
    for extra_line in name_lines[1:]:
        line("{:<30}".format(extra_line))

    # Print quantity and unit price line
    #qty_line = f"  {qty} @ {unit_price:,.2f}"
    #line("{:<30}".format(qty_line))

# === Payments ===
total = f"{receipt['receiptTotal']:,.2f}"
receiptPayments = receipt.get("receiptPayments", [])

# Initialize variables for payment method and amount
payment_method = ""
paymentAmount = ""

# Assuming that there's only one payment method in the list
if receiptPayments:
    payment_method = receiptPayments[0].get("moneyTypeCode", "")
    paymentAmount = f"{receiptPayments[0].get('paymentAmount', 0):,.2f}"  # Ensure the amount is formatted correctly

# Print the total and payment method
line(bold_line)
line("{:<30}{:>15}".format("Total", total))
line("{:<30}{:>15}".format(payment_method, paymentAmount)) 

# Process each payment
for payment in receipt.get("payments", []):
    line("{:<30}{:>15}".format(payment.get("paymentMethod", ""), f"{payment.get('amount', 0):,.2f}"))

#Number of Items paid
line(bold_line)
items_paid = len(receipt["receiptLines"])
line("{:<30}{:>15}".format("Number of items: ", items_paid)) 
line(bold_line)

# === Tax Summary === 
for tax in receipt.get("receiptTaxes", []):
    # Extract values from the tax item
    tax_code = tax.get('taxCode', 'Unknown')
    tax_rate = f"{tax.get('taxPercent', 0)}%"
    taxable_amount = tax.get('salesAmountWithTax', 0)
    tax_amount = tax.get('taxAmount', 0)
    net_amount = taxable_amount - tax_amount

    # Print the summary in a structured format simple without the bold text
    #line("{:<30}{:>15}".format("Net Amount"), f"{taxable_amount:,.2f}")
    #line("{:<30}{:>15}".format(f"VAT ({tax_rate})"), f"{tax_amount:,.2f}")
    #line("{:<30}{:>15}".format("Gross Amount"), f"{net_amount:,.2f}")
    #line(bold_line)

    # draw bold label
    c.setFont("Helvetica-Bold", 8)
    c.drawString(x_pos, y, "Net Amount")  # x_pos is your left margin

    # draw normal number
    c.setFont("Courier", 8)
    c.drawRightString(right_x, y, f"{taxable_amount:,.2f}")
    y -= line_height

    c.setFont("Helvetica-Bold", 8)
    c.drawString(x_pos, y, f"VAT ({tax_rate})")
    c.setFont("Courier", 8)
    c.drawRightString(right_x, y, f"{tax_amount:,.2f}")
    y -= line_height

    c.setFont("Helvetica-Bold", 8)
    c.drawString(x_pos, y, "Gross Amount")
    c.setFont("Courier", 8)
    c.drawRightString(right_x, y, f"{net_amount:,.2f}")
    y -= line_height
    line(bold_line)

# === Footer ===
line("Invoice is issued after purchasing goods", 8)
y -= 150  # Adjust y position for QR code
qr_code_size = 100  # Increase size for the QR code
qr_x = (receipt_width - qr_code_size) / 2  # Centering the QR code
if os.path.exists(qr_path):
    c.drawImage(qr_path, qr_x, y, width=qr_code_size, height=qr_code_size)

y -= qr_code_size - 100  # Move down after the QR code

# Center the verification label
c.setFont("Courier", 8)
c.drawCentredString(center_x, y, "Verification Code:")
y -= 10
c.drawCentredString(center_x, y, f"{qr_data}")
y -= 20

# Link text
c.drawCentredString(center_x, y, "You can verify this receipt manually at:")
y -= 10

# Centralized and clickable hyperlink
c.setFont("Courier", 8)
hyperlink_centered(qr_url, qr_url)
y -= 20

# === Save PDF ===
c.save()
print("✅ Receipt48 generated")


