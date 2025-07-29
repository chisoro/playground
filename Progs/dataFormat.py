#Data Formatting.
#Design/Written:....................Simbarashe chisoro
#Date:............................29 July 2025

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
from flask_login import LoginManager, UserMixin, login_user, logout_user
from flask_bcrypt import bcrypt
import json
import psycopg2
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import landscape,A4
from reportlab.lib.pagesizes import letter
from reportlab.lib import colors
import os
import csv
import re
import pyqrcode
#import png
from pyqrcode import QRCode
import ast
from datetime import date
from datetime import datetime
import requests
import hashlib
import base64
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.hazmat.primitives.asymmetric.utils import encode_dss_signature
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import padding

def format_receipt_total(value):
    # Convert float receipt total to integer cents
    return str(int(round(value * 100)))

def format_tax_percent(tax_percent):
    # Return empty string if tax_percent is None (exempt)
    if tax_percent is None:
        return ""
    # Format with two decimals (e.g. 15.00, 14.50, 0.00)
    return f"{tax_percent:.2f}"

def format_receipt_date(day):
    date_format = "%d/%m/%y"
    rd = datetime.strptime(day, date_format)
    rd = rd.isoformat()
    return rd

def format_sales_eod(sales):
    def sort_key(t):
        tax_code = t['taxCode'] or ''
        return (t['taxID'], '' if tax_code == '' else tax_code)

    sorted_sales = sorted(sales, key=sort_key)

    result = ""
    result1 =""
    for sale in sorted_sales:
        sale_by_tax = "SaleByTax".upper()
        sale_by_tax_by_tax = "SaleTaxByTax".upper()
        tax_cur = sale['taxCur']
        tax_percent = format_tax_percent(sale['taxPercent'])
        tax_amount = str(int(round(sale['taxAmount']*100)))
        sales_amount = str(int(round(sale['salesAmountWithTax']*100)))
        result += sale_by_tax + tax_cur + tax_percent + sales_amount
        result += sale_by_tax_by_tax + tax_cur + tax_percent + tax_amount

    print(result, result1)

def format_receipt_taxes(taxes):
    def sort_key(t):
        # taxCode empty strings sort before others
        tax_code = t['taxCode'] or ''
        return (t['taxID'], '' if tax_code == '' else tax_code)


    sorted_taxes = sorted(taxes, key=sort_key)


    result = ""
    for tax in sorted_taxes:
        tax_code = tax['taxCode'] or ""
        tax_percent = format_tax_percent(tax['taxPercent'])
        tax_amount = str(int(round(tax['taxAmount']*100)))
        sales_amount = str(int(round(tax['salesAmountWithTax']*100)))
        result += tax_code + tax_percent + tax_amount + sales_amount

    return result


def concatenate_fields(fields):
    # Concatenate without separator in order
    return "".join(fields)

def generate_hash(concatenated_string):
    return hashlib.sha256(concatenated_string.encode('utf-8')).hexdigest()

def sign_concatenated_string(private_key, concatenated_string):
    # Sign using RSA PKCS#1 v1.5 padding with SHA256 hash algorithm
    signature = private_key.sign(
        concatenated_string.encode('utf-8'),
        padding.PKCS1v15(),
        hashes.SHA256()
    )
    return signature.hex()  # Return hex signature for display/storage

def generate_sales_fields(sales):
    fields =[]
    fields.append((format_sales_eod(sales)))
    print(fields)
    return fields
