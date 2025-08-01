import json
from datetime import datetime
from reportlab.pdfgen import canvas
from reportlab.lib.units import mm

#Auther: Coleen Marasha
#Modified: Simbarashe Chisoro

def gen_report(receipt_path, refile):
    margin = 10 * mm
    width = 100 * mm
    height = 500 * mm
    pagesize = (width, height)
    work_file = f'{receipt_path}{refile}.json'
    with open(work_file, "r") as f:
        eod_wrapper = json.load(f)
        eod_data = eod_wrapper.get("EOD", {})

    # Extract main fields
    fiscal_day_no = eod_data.get("fiscalDayNo", "N/A")
    opened = eod_data.get("opened", "")
    closed = eod_data.get("closed", "")
    device_id = eod_data.get("deviceID", "N/A")
    device_serial = eod_data.get("serial", "N/A")
    seller = eod_data.get("sellerData", {})
    company_name = seller.get("sellerRegisterName", "N/A")
    tin = seller.get("sellerTIN", "N/A")
    vat_number = seller.get("sellervatNumber", "N/A")
    contacts = seller.get("sellerContacts", {})

    # Parse dates
    opened_dt = datetime.strptime(opened, "%d/%m/%y") if opened else None
    closed_dt = datetime.strptime(closed, "%d/%m/%y") if closed else None

    # Prepare PDF
    pdf_path = f'{receipt_path}{refile}.pdf'
    c = canvas.Canvas(pdf_path, pagesize=pagesize)
    y = [height - margin]

    def line_total(label, value, offset=12):
        c.drawString(margin, y[0], f"{label}")
        c.drawRightString(width - margin, y[0], f"{value:,.2f}")
        y[0] -= offset

    def line(txt="", offset=12):
        c.drawString(10 * mm, y[0], txt)
        y[0] -= offset

    def bold_line(offset=12):
        line_width = int((width - 2 * margin) / 5)
        c.drawString(margin, y[0], "=" * line_width)
        y[0] -= offset

    def dot_line(offset=12):
        line_width = int((width - 2 * margin) / 5)
        c.drawString(margin, y[0], "-" * line_width)
        y[0] -= offset

    def centered_line(text, offset=12):
        c.drawCentredString(width / 2, y[0], text)
        y[0] -= offset

    # Header
    c.setFont("Courier-Bold", 10)
    centered_line(f"{company_name}")
    centered_line(f"TIN: {tin}")
    centered_line(f"VAT Number: {vat_number}")
    y[0] -= 6
    line(f" {contacts.get('sellerStreet', '')}, {contacts.get('sellerCity', '')}")
    centered_line(f" {contacts.get('selleremail', '')}")
    centered_line(f" {contacts.get('sellerphoneNo', '')}")
    dot_line()
    y[0] -= 6

    c.setFont("Courier-Bold", 14)
    centered_line("Z REPORT")
    y[0] -= 8

    c.setFont("Courier-Bold", 10)
    dot_line()
    line(f"Fiscal Day No: {fiscal_day_no}")
    if opened_dt:
        line(f"Fiscal Day opened: {opened_dt.strftime('%d/%m/%Y')}")
    if closed_dt:
        line(f"Fiscal Day closed: {closed_dt.strftime('%d/%m/%Y')}")
    line(f"Device Serial No: {device_serial}")
    line(f"Device Id: {device_id}")
    dot_line()

    c.setFont("Courier-Bold", 12)
    centered_line("Daily totals")
    c.setFont("Courier-Bold", 10)
    dot_line()

    # Process Sales per Currency
    sales = eod_data.get("Sales", [])
    currencies = sorted(set(s["taxCur"] for s in sales if s.get("taxCur")))

    for cur in currencies:
        c.setFont("Helvetica-Bold", 10)
        line(cur, 15)
        c.setFont("Courier-Bold", 10)
        dot_line()

        # === Net Sales ===
        c.setFont("Helvetica-Bold", 10)
        line("Total Net Sales")
        c.setFont("Courier-Bold", 10)

        tax_percents = sorted(set(
            s.get("taxPercent") for s in sales if s.get("taxCur") == cur
        ), reverse=True)  #descending order

        total_net = 0
        for percent in tax_percents:
            label = f"Net, VAT {percent}%" if percent >= 0 else "Net, Non-VAT"
            net = sum(
                s["salesAmountWithTax"] - s["taxAmount"]
                for s in sales if s.get("taxCur") == cur and s.get("taxPercent") == percent
            )
            line_total(label, net)
            total_net += net
        c.setFont("Helvetica-Bold", 10)
        line_total("Total Net Amount", total_net)

        # === Taxes ===
        dot_line()
        c.setFont("Helvetica-Bold", 10)
        line("Total Taxes")
        c.setFont("Courier-Bold", 10)

        total_tax = 0
        for percent in tax_percents:
            tax = sum(
                s["taxAmount"]
                for s in sales if s.get("taxCur") == cur and s.get("taxPercent") == percent
            )
            line_total(f"Total, VAT {percent}%", tax)
            total_tax += tax
        c.setFont("Helvetica-Bold", 10)
        line_total("Total Tax Amount", total_tax)

        # === Gross Sales ===
        dot_line()
        c.setFont("Helvetica-Bold", 10)
        line("Total Gross Sales")
        c.setFont("Courier-Bold", 10)

        total_gross = 0
        for percent in tax_percents:
            label = f"Net, VAT {percent}%" if percent else "Net, Non-VAT, 0%"
            #label = f"Total, VAT {percent}%" if percent > 0 else "Total, Non-VAT 0%"
            gross = sum(
                s["salesAmountWithTax"]
                for s in sales if s.get("taxCur") == cur and s.get("taxPercent") == percent
            )
            line_total(label, gross)
            total_gross += gross
        c.setFont("Helvetica-Bold", 10)
        line_total("Total Gross Amount", total_gross)

        # === Document Summary ===
        dot_line()
        c.setFont("Helvetica-Bold", 10)
        line(f"{'Documents':<15}{'Quantity':>16}{'Total amount':>26}", 12)
        c.setFont("Courier-Bold", 10)

        dq = [q for q in eod_data.get("DayQuantities", []) if q.get("taxCur", "").upper() == cur.upper()]
        invoice_qty = sum(q.get("Quantity", 0) for q in dq if q.get("Type", "").lower() == "invoice")
        invoice_amt = sum(q.get("Amount", 0.0) for q in dq if q.get("Type", "").lower() == "invoice")
        credit_qty = sum(q.get("Quantity", 0) for q in dq if q.get("Type", "").lower() == "credit")
        credit_amt = sum(q.get("Amount", 0.0) for q in dq if q.get("Type", "").lower() == "credit")
        debit_qty = sum(q.get("Quantity", 0) for q in dq if q.get("Type", "").lower() == "debit")
        debit_amt = sum(q.get("Amount", 0.0) for q in dq if q.get("Type", "").lower() == "debit")

        total_qty = invoice_qty + credit_qty + debit_qty
        total_amt = invoice_amt + credit_amt + debit_amt

        line(f"{'Invoices':<15}{invoice_qty:>6}{invoice_amt:>18,.2f}")
        line(f"{'Credit notes':<15}{credit_qty:>6}{credit_amt:>18,.2f}")
        line(f"{'Debit notes':<15}{debit_qty:>6}{debit_amt:>18,.2f}")
        line(f"{'Total documents':<15}{total_qty:>6}{total_amt:>18,.2f}")
        bold_line()

    c.save()
    return pdf_path
