# Author: Colleen Marasha
import json
from datetime import datetime
from reportlab.pdfgen import canvas
from reportlab.lib.units import mm
import textwrap



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
        line_width = int((width - 2 * margin) / 5)  # approx char width
        c.drawString(margin, y[0], "-" * line_width)
        y[0] -= offset

    def centered_line(text, offset=12):
        c.drawCentredString(width / 2, y[0], text)
        y[0] -= offset

    # === Company Info ===
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

    # === Title ===
    c.setFont("Courier-Bold", 14)
    c.drawCentredString(width / 2, y[0], "Z REPORT")
    y[0] -= 8

    # === Fiscal Day Info ===
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

    # === Sub Title ===
    c.setFont("Courier-Bold", 12)
    c.drawCentredString(width / 2, y[0], "Daily totals")
    c.setFont("Courier-Bold", 10)
    y[0] -= 8
    dot_line()

    # === Currency Sections ===
    currencies = set(
        sale.get("taxCur") for sale in eod_data.get("Sales", []) if sale.get("taxCur")
    )
    for cur in sorted(currencies):
        c.setFont("Helvetica-Bold", 10)
        line(cur, 15)
        c.setFont("Courier-Bold", 10)
        dot_line()
        c.setFont("Helvetica-Bold", 10)
        line("Total Net Sales")
        c.setFont("Courier-Bold", 10)

        net_vat_15 = sum(
            s["salesAmountWithTax"] - s["taxAmount"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 15
        )

        net_vat_9 = sum(
            s["salesAmountWithTax"] - s["taxAmount"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 9
        )

        net_non_vat = sum(
            s["salesAmountWithTax"] - s["taxAmount"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 0
        )

        net_exempt = 0.0

        total_net = net_vat_15 + net_vat_9 + net_non_vat + net_exempt

        line_total("Net, VAT 15%", net_vat_15)
        line_total("Net, VAT 9%", net_vat_9)
        line_total("Net, Non-VAT, 0%", net_non_vat)
        line_total("Net, Exempt", net_exempt)

        c.setFont("Helvetica-Bold", 10)
        line_total("Total Net Amount", total_net)
        c.setFont("Courier-Bold", 10)

        dot_line()
        c.setFont("Helvetica-Bold", 10)
        line("Total Taxes")
        c.setFont("Courier-Bold", 10)

        total_vat_15 = sum(
            s["taxAmount"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 15
        )
        total_vat_9 = sum(
            s["taxAmount"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 9
        )
        total_tax = total_vat_15 + total_vat_9

        line_total("Total, VAT 15%", total_vat_15)
        line_total("Total, VAT 9%", total_vat_9)
        c.setFont("Helvetica-Bold", 10)
        line_total("Total Tax Amount", total_tax)
        c.setFont("Courier-Bold", 10)
        dot_line()

        c.setFont("Helvetica-Bold", 10)
        line("Total Gross Sales")
        c.setFont("Courier-Bold", 10)

        gross_vat_15 = sum(
            s["salesAmountWithTax"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 15
        )
        gross_vat_9 = sum(
            s["salesAmountWithTax"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 9
        )
        gross_non_vat = sum(
            s["salesAmountWithTax"]
            for s in eod_data.get("Sales", [])
            if s.get("taxCur") == cur and s.get("taxPercent") == 0
        )
        gross_exempt = 0.0  # Update if exempt logic/data is available
        total_gross = gross_vat_15 + gross_vat_9 + gross_non_vat + gross_exempt

        line_total("Total, VAT 15%", gross_vat_15)
        line_total("Total, VAT 9%", gross_vat_9)
        line_total("Total, Non-VAT 0%", gross_non_vat)
        line_total("Total, Exempt", gross_exempt)
        c.setFont("Helvetica-Bold", 10)
        line_total("Total Gross Amount", total_gross)
        c.setFont("Courier-Bold", 10)
        dot_line()

        c.setFont("Helvetica-Bold", 10)
        line(f"{'Documents':<15}{'Quantity':>16}{'Total amount':>26}", 12)
        c.setFont("Courier-Bold", 10)

        day_quantities = [
            q
            for q in eod_data.get("DayQuantities", [])
            if q.get("taxCur", "").strip().upper() == cur.upper()
        ]

        invoice_qty = sum(q.get("Quantity", 0) for q in day_quantities if q.get("Type", "").lower() == "invoice")
        invoice_amt = sum(q.get("Amount", 0.0) for q in day_quantities if q.get("Type", "").lower() == "invoice")

        credit_qty = sum(q.get("Quantity", 0) for q in day_quantities if q.get("Type", "").lower() == "credit")
        credit_amt = sum(q.get("Amount", 0.0) for q in day_quantities if q.get("Type", "").lower() == "credit")

        debit_qty = sum(q.get("Quantity", 0) for q in day_quantities if q.get("Type", "").lower() == "debit")
        debit_amt = sum(q.get("Amount", 0.0) for q in day_quantities if q.get("Type", "").lower() == "debit")

        total_qty = invoice_qty + credit_qty + debit_qty
        total_amt = invoice_amt + credit_amt + debit_amt

        line(f"{'Invoices':<15}{invoice_qty:>6}{invoice_amt:>18,.2f}")
        line(f"{'Credit notes':<15}{credit_qty:>6}{credit_amt:>18,.2f}")
        line(f"{'Debit notes':<15}{debit_qty:>6}{debit_amt:>18,.2f}")

        line(f"{'Total documents':<15}{total_qty:>6}{total_amt:>18,.2f}")
        bold_line()

    # Save PDF
    c.save()
    return pdf_path
