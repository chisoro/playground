# Author: Colleen Marasha
import json
import re
from datetime import datetime
from hashlib import md5
import base64
import qrcode
from PIL import Image
import os
import textwrap
from textwrap import wrap
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.lib.units import mm

# === Load JSON Data ===
def load_clean_json(path):
    with open(path, "r") as f:
        raw = f.read()
    cleaned = re.sub(r",\s*([}\]])", r"\1", raw)
    return json.loads(cleaned)

receipt = load_clean_json("receipt.json")["receipt"]
with open("recresponse.json") as f:
    rec_response = json.load(f)

# === Format Date ===
def format_date(date_str):
    return datetime.strptime(date_str, "%Y-%m-%dT%H:%M:%S").strftime("%d/%m/%y %H:%M")

# === Build QR Code String ===
qr_url = rec_response.get("qrUrl", "https://invoice.zimra.co.zw/")
device_id = str(receipt.get("deviceID") or receipt.get("creditDebitNote", {}).get("deviceID", 0)).zfill(10)
receipt_date = datetime.strptime(receipt["receiptDate"], "%Y-%m-%dT%H:%M:%S").strftime("%d%m%Y")
receipt_global_no = str(receipt["receiptGlobalNo"]).zfill(10)
sig_hash = rec_response.get("receiptDeviceSignature", {}).get("hash") or            rec_response.get("receiptServerSignature", {}).get("hash")

# ZIMRA receipt device signature string wants md5. To handle base64 hash
try:
    raw_bytes = base64.b64decode(sig_hash)
    qr_data = md5(raw_bytes).hexdigest().upper()[:16]
except Exception as e:
    raise ValueError(f"Invalid signature hash format: {e}")

qr_string = f"{qr_url}{device_id}{receipt_date}{receipt_global_no}{qr_data}"

# === Generate QR Code ===
qr_img = qrcode.make(qr_string)
qr_path = "qr_code_zimra.png"
qr_img.save(qr_path)

# === Create PDF ===
pdf_path = "InvoiceA4.pdf"
c = canvas.Canvas(pdf_path, pagesize=A4)
width, height = A4
margin = 50

# === Header ===
if os.path.exists("logo.png"):
    c.drawImage("logo.png", margin, height - 120, width=80, height=80)


# === QR & Verification Code ===
c.setFont("Helvetica", 9)
c.drawRightString(width - 140, height - 50, "Verification code")
c.setFont("Helvetica-Bold", 10)
c.drawRightString(width - 140, height - 65, f"{qr_data[:4]}-{qr_data[4:8]}-{qr_data[8:12]}-{qr_data[12:16]}")
c.setFont("Helvetica", 8)
c.drawRightString(width - 140, height - 80, "You can verify this receipt manually at")
c.drawRightString(width - 140, height - 92, qr_url)
c.drawImage(qr_path, width - 140, height - 120, width=100, height=100)

c.setFont("Helvetica-Bold", 16)
c.drawCentredString(width / 2, height - 160, "FISCAL TAX INVOICE")

line_y = height - 180

# === Seller ===
from textwrap import wrap

# === Seller ===
seller = receipt.get("sellerData", {})
c.setFont("Helvetica-Bold", 9)
c.drawString(margin, line_y - 10, "SELLER")
c.setFont("Helvetica", 9)

wrap_width = 50  # Approx. number of characters in half the page
line_spacing = 12
y_offset = line_y - 25
for field in [
    seller.get("taxpayerName", ""),
    f"TIN: {seller.get('taxpayerTIN', '')}",
    f"VAT No: {seller.get('vatNumber', '')}",
    seller.get("deviceBranchName", ""),
    seller.get("deviceBranchAddress", ""),
    seller.get("deviceBranchContacts", {}).get("email", ""),
    seller.get("deviceBranchContacts", {}).get("phoneNo", "")
]:
    for wrapped_line in wrap(field, wrap_width):
        c.drawString(margin, y_offset, wrapped_line)
        y_offset -= line_spacing

# === Buyer ===
buyer = receipt.get("buyerData", {})
c.setFont("Helvetica-Bold", 9)
c.drawString(width / 2, line_y - 10, "BUYER")
c.setFont("Helvetica", 9)

buyer_x = width / 2
y_offset = line_y - 25
for field in [
    buyer.get("buyerRegisterName", ""),
    buyer.get("buyerTradeName", ""),
    f"TIN: {buyer.get('buyerTIN', '')}",
    buyer.get("buyerAddress", {}).get("street", ""),
    buyer.get("buyerContacts", {}).get("email", ""),
    buyer.get("buyerContacts", {}).get("phoneNo", "")
]:
    for wrapped_line in wrap(field, wrap_width):
        c.drawString(buyer_x, y_offset, wrapped_line)
        y_offset -= line_spacing

# === Invoice Info ===
info_y = line_y - 130
c.line(margin, info_y + 10, width - margin, info_y + 10)
c.setFont("Helvetica", 9)
c.drawString(margin, info_y, f"Invoice No: {receipt['receiptCounter']} / {receipt['receiptGlobalNo']}")
c.drawString(margin + 200, info_y, f"Fiscal Day No: {receipt['fiscalDayNo']}")
info_y -= 15
c.drawString(margin, info_y, f"Customer ref No: {receipt['invoiceNo']}")
c.drawString(margin + 200, info_y, f"Date: {format_date(receipt['receiptDate'])}")
info_y -= 15
c.drawString(margin, info_y, f"Device Serial No: {receipt['deviceSerialNo']}")
c.drawString(margin + 200, info_y, f"Device ID: {receipt.get('deviceID', 0)}")
info_y = line_y -180
c.line(margin, info_y + 10, width - margin, info_y + 10)


# === Table Grid ===
table_data = [
    ["Code", "Description", "Qty", "Price", "VAT", "Total Amount (incl. tax)"]
]
for line in receipt["receiptLines"]:
    table_data.append([
        line.get("receiptLineCode", ""),
        line.get("receiptLineName", "")[:30],
        str(line.get("receiptLineQuantity", "")),
        f"{line.get("receiptLinePrice", 0):,.2f}",
        f"{line.get("taxAmount", 0):,.2f}" if line.get("taxAmount", 0) > 0 else "-",
        f"{line.get("receiptLineTotal", 0):,.2f}"
    ])

col_widths = [50, 150, 40, 60, 60, 80]
x_positions = [margin]
for width in col_widths:
    x_positions.append(x_positions[-1] + width)
table_y = info_y - 30
row_height = 15
current_y = table_y

for row in table_data:
    c.rect(x_positions[0], current_y - row_height, x_positions[-1] - x_positions[0], row_height, stroke=1, fill=0)
    for i in range(1, len(x_positions) - 1):
        c.line(x_positions[i], current_y, x_positions[i], current_y - row_height)
    for i, text in enumerate(row):
        if i > 1:
            c.drawRightString(x_positions[i + 1] - 5, current_y - 11, text)
        else:
            c.drawString(x_positions[i] + 2, current_y - 11, text)
    current_y -= row_height

# === Totals (Separate Section) ===
total_tax = sum(t.get("taxAmount", 0) for t in receipt.get("receiptTaxes", []))
current_y -= 10
c.setFont("Helvetica", 9)
c.drawRightString(x_positions[4] + col_widths[4] - 5, current_y, "Total15% VAT")
c.drawRightString(x_positions[5] + col_widths[5] - 5, current_y, f"{total_tax:,.2f}")
current_y -= 15
c.setFont("Helvetica-Bold", 10)
c.drawRightString(x_positions[4] + col_widths[4] - 5, current_y, "Invoice total")
c.drawRightString(x_positions[5] + col_widths[5] - 5, current_y, f"{receipt['receiptTotal']:,.2f}")
#underline totals
#current_y -= 10
#c.line(margin, current_y, width - margin, current_y) small line left

# === Footer ===
footer_y = current_y - 30
c.setFont("Helvetica", 8)
c.drawString(margin, footer_y, "Invoice is issued after purchasing goods according to agreement No555")

# === Save PDF ===
c.save()
print("✅ ZIMRA InvoiceA4 generated with separate totals section.")
