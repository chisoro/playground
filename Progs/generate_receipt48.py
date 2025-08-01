# Author: Colleen Marasha
#Modified: Simbarashe Chisoro
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

def load_clean_json(path):
    with open(path, "r") as f:
        raw = f.read()
    cleaned = re.sub(r",\s*([}\]])", r"\1", raw)
    return json.loads(cleaned)

def gen_report78(receipt_path, refile):
    print("hello")

def gen_report(receipt_path, refile):
    # Set up default positions
    x_pos = 2 * mm               # left margin
    right_x = 75 * mm            # right margin
    line_height = 12             # space between lines

    data = load_clean_json(f'{receipt_path}{refile}.json')
    receipt = data["receipt"]
    #receipt = load_clean_json(f'{receipt_path}{refile}.json')["receipt"]
    sig = data["receiptDeviceSignature"]
    # === Generate QR Code Hash ===
    qr_url = receipt["qUrl"]
    #qr_url = rec_response.get("qrUrl", "https://invoice.zimra.co.zw/")
    #device_id = str(receipt.get("deviceID") or receipt.get("creditDebitNote", {}).get("deviceID", 0)).zfill(10)
    device_id =str(receipt["deviceID"]).zfill(10)
    receipt_date = datetime.strptime(receipt["receiptDate"], "%Y-%m-%dT%H:%M:%S").strftime("%d%m%Y")
    receipt_global_no = str(receipt["receiptGlobalNo"]).zfill(10)
    #sig_hash = rec_response.get("receiptDeviceSignature", {}).get("hash") or            rec_response.get("receiptServerSignature", {}).get("hash")
    sig_hash = sig["signature"]

    # ZIMRA receipt device signature string wants md5. To handle base64 hash
    try:
        raw_bytes = base64.b64decode(sig_hash)
        qr_data = md5(raw_bytes).hexdigest().upper()[:16]
    except Exception as e:
        raise ValueError(f"Invalid signature hash format: {e}")

    qr_string = f"{qr_url}{device_id}{receipt_date}{receipt_global_no}{qr_data}"

    # === Generate QR Code Image ===
    qr_img = qrcode.make(qr_string)
    qr_path = "qr_receipt48.png"
    qr_img.save(qr_path)

    # === Create Receipt PDF ===
    receipt_width = 80 * mm
    receipt_height = 300 * mm
    pdf_path = f'{receipt_path}{refile}.pdf'
    c = canvas.Canvas(pdf_path, pagesize=(receipt_width, receipt_height))
    c.setFont("Courier", 8)

    x = 5
    y = [receipt_height - 10]

    def line(txt="", offset=12, font="Courier", size=8):
        c.setFont(font, size)
        c.drawString(x_pos, y[0], txt)
        y[0] -= offset

    def hyperlink(text, url, offset=12):
        c.setFillColorRGB(0, 0, 1)  # blue
        c.drawString(x, y[0], text)
        c.linkURL(url, (x, y[0] - 1, x + 150, y[0] + 10), relative=1)
        c.setFillColorRGB(0, 0, 0)  # reset to black
        y[0] -= offset


    def hyperlink_centered(text, url, font="Courier", size=8):
        c.setFont(font, size)
        text_width = c.stringWidth(text, font, size)
        c.setFillColorRGB(0, 0, 1)
        c.drawCentredString(center_x, y[0], text)
        c.linkURL(url, (center_x - text_width / 2, y[0] - 2, center_x + text_width / 2, y[0] + 8), relative=0)
        c.setFillColorRGB(0, 0, 0)
        y[0] -= 12

    bold_line = "-" * 48

    # === Optional Logo ===
    if os.path.exists("logo.png"):
        c.drawImage("logo.png", (receipt_width - 40) / 2, y[0] - 40, width=40, height=40)
        y[0] -= 50
    if os.path.exists("logo.jpg"):
        c.drawImage("logo.jpg", (receipt_width - 40) / 2, y[0] - 40, width=40, height=40)
        y[0] -= 50
    center_offset = 15  # Space after centered text


    # === Seller Info ===
    seller = receipt.get("sellerData", {})
    receipt_width = 80 * mm
    receipt_height = 300 * mm
    margin = 30
    center_x = receipt_width / 2

    taxpayer_name = seller.get("sellerTradeName", "")
    tin = f"TIN: {seller.get('taxpayerTIN', '')}"
    vat_no = f"VAT No: {seller.get('sellervatNumber', '')}"

    # Draw the centered strings under company logo
    c.drawCentredString(center_x, receipt_height - 60, taxpayer_name)
    c.drawCentredString(center_x, receipt_height - 75, tin)
    c.drawCentredString(center_x, receipt_height - 90, vat_no)
    y[0] -= (center_offset + 30)

    line(seller.get("sellerContacts", {}).get( "sellerHouse", ""))
    line(seller.get("sellerContacts", {}).get("sellerStreet", ""))
    line(seller.get("sellerContacts", {}).get("sellerCity", ""))
    email = seller.get("sellerContacts", {}).get("selleremail", "")
    if email:
        hyperlink(email, f"mailto:{email}")
    else:
        line("")
    line(seller.get("sellerContacts", {}).get("sellerphoneNo", ""))
    line(bold_line)

    # === FISCAL TAX INVOICE TITLE ===
    y[0] -= 5
    c.setFont("Courier-Bold", 14)
    c.drawCentredString(receipt_width / 2, y[0], "FISCAL TAX INVOICE")
    y[0] -= 15
    c.setFont("Courier", 8)
    line(bold_line)


    # === Buyer Info ===
    buyer = receipt.get("buyerData", {})

    c.setFont("Courier-Bold", 8)
    c.drawCentredString(center_x, y[0], "BUYER:")
    y[0] -= 12
    c.setFont("Courier", 8)
    for field in [
        buyer.get("buyerRegisterName", ""),
        f"TIN: {buyer.get('buyerTIN', '')}",
        f"House No: {buyer.get("buyerAddress", {}).get("houseNo", "")}",
        buyer.get("buyerAddress", {}).get("street", ""),
        buyer.get("buyerAddress", {}).get("city", ""),
        buyer.get("buyerContacts", {}).get("buyerphoneNo", "")
    ]:
        if field:
            c.drawCentredString(center_x, y[0], field)
            y[0] -= 12

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
        c.drawString(x_pos, y[0], "Net Amount")  # x_pos is your left margin

        # draw normal number
        c.setFont("Courier", 8)
        c.drawRightString(right_x, y[0], f"{net_amount:,.2f}")
        y[0] -= line_height

        c.setFont("Helvetica-Bold", 8)
        c.drawString(x_pos, y[0], f"VAT ({tax_rate})")
        c.setFont("Courier", 8)
        c.drawRightString(right_x, y[0], f"{tax_amount:,.2f}")
        y[0] -= line_height

        c.setFont("Helvetica-Bold", 8)
        c.drawString(x_pos, y[0], "Gross Amount")
        c.setFont("Courier", 8)
        c.drawRightString(right_x, y[0], f"{taxable_amount:,.2f}")
        y[0] -= line_height
        line(bold_line)

    # === Footer ===
    #line("Invoice is issued after purchasing goods", 8)
    line("{:<30}{:>15}".format("Receipt Currency: ", receipt['receiptCurrency']))
    line("{:<30}{:>15}".format("USD Rate: ", receipt['rateUSD']))
    line("{:<30}{:>15}".format("Balance Before: ", receipt['balBF']))
    line("{:<30}{:>15}".format("Uposted Receipts: ", receipt['unPosted']))
    line("{:<30}{:>15}".format("Balance After: ", receipt['balBF'] - receipt['unPosted'] - receipt['receiptTotal']*(receipt['rateRecCur']/ receipt['rateUSD'])))
    y[0] -= 90  # Adjust y position for QR code
    qr_code_size = 100  # Increase size for the QR code
    qr_x = (receipt_width - qr_code_size) / 2  # Centering the QR code
    if os.path.exists(qr_path):
        c.drawImage(qr_path, qr_x, y[0], width=qr_code_size, height=qr_code_size)

    y[0] -= qr_code_size - 100  # Move down after the QR code

    # Center the verification label
    c.setFont("Courier", 8)
    c.drawCentredString(center_x, y[0], "Verification Code:")
    y[0] -= 10
    c.drawCentredString(center_x, y[0], f"{qr_data[:4]}-{qr_data[4:8]}-{qr_data[8:12]}-{qr_data[12:16]}")
    y[0] -= 20

    # Link text
    c.drawCentredString(center_x, y[0], "You can verify this receipt manually at:")
    y[0] -= 10

    # Centralized and clickable hyperlink
    c.setFont("Courier", 8)
    hyperlink_centered(qr_url, qr_url)
    y[0] -= 20

    # === Save PDF ===
    c.save()
    print("✅ Receipt48 generated")
