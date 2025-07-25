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



app = Flask(__name__)

#api interface to simacc system and ZIMRA FDMS
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

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

    """
    def sort_key(t):
        tax_id = t['taxID']
        return (t['taxID'])
    """
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

    """
    for tax in sorted_taxes:
        tax_id =  ""
        tax_percent = format_tax_percent(tax['taxPercent'])
        tax_amount = str(int(tax['taxAmount']))
        sales_amount = str(int(tax['salesAmountWithTax']))
        result += str(tax_id) + tax_percent + tax_amount + sales_amount
    """

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

def generate_signature_and_hash(receipt, private_key):
    # Prepare fields
    fields = []
    fields.append(str(receipt['deviceID']))
    fields.append(receipt['receiptType'].upper())
    fields.append(receipt['receiptCurrency'].upper())
    fields.append(str(receipt['receiptGlobalNo']))
    fields.append(receipt['receiptDate'])  # already in ISO 8601 format
    fields.append(format_receipt_total(receipt['receiptTotal']))
    fields.append(format_receipt_taxes(receipt['receiptTaxes']))

    # Include previousReceiptHash if present (not first receipt in fiscal day)
    if receipt['receiptCounter'] != 1:
        previous_receipt_hash = receipt['pHash']
    else:
        previous_receipt_hash = None

    if previous_receipt_hash:
        fields.append(previous_receipt_hash)

    concatenated = concatenate_fields(fields)
    print(concatenated)
    receipt_hash = generate_hash(concatenated)
    signature = sign_concatenated_string(private_key, concatenated)

    return receipt_hash, signature


@app.route('/register/<deviceid>/<ver>/<ack>/<dvname>', methods=['GET'])
def get_reg(deviceid,ack,dvname,ver):
    mycsv = csv.reader(open('C:\\Users\\ADMIN\\OneDrive\\Documents\\Projects\\Progress\\Simacc\\conn\\conn.csv'))
    for row in mycsv:
        f = row[0]
    mycert = csv.reader(open(f+"zimra\\scr"+deviceid+".csv"))
    for row in mycert:
        scr = row[0]

    url = "https://fdmsapitest.zimra.co.zw/Public/" + ver + "/" + deviceid +"/RegisterDevice"

    payload = json.dumps({
      "certificateRequest": scr,
      "activationKey": ack
    })
    headers = {
      'Accept': 'application/json',
      'DeviceModelName': dvname,
      'DeviceModelVersion': ver,
      'Content-Type': 'application/json'
    }

    r = requests.request("POST", url, headers=headers, data=payload)

    if r == "[]":
        return jsonify([{'rsMessage':"We cannot reach the api."}])
    else:
        #return r.text
        return r.text



@app.route('/getConfig/<deviceid>/<model>/<version>', methods=['GET'])
def get_conf(deviceid,model,version):

    client_cert = 'C:\\Users\\ADMIN\\client_cert.pem'  # Self-signed client certificate noot used in this situation
    client_key = 'C:\\Users\\ADMIN\\private_key.pem'    # Private key for the client certificate
    server_cert = 'C:\\Users\\ADMIN\\server_cert.pem'   #zimra server signed certificate

    url = 'https://fdmsapitest.zimra.co.zw/Device/v1/' + deviceid + '/GetConfig'  # API URL

    headers = {
        'accept': 'application/json',
        'DeviceModelName': model,
        'DeviceModelVersion': version
    }

    try:
        response = requests.get(
            url,
            headers=headers,
            cert=(server_cert, client_key),
            verify=False
            #verify='C:\\Users\\ADMIN\\server_cert.pem'  # CA certificate for server verification
            #Note: Not sure if this is true mTLS as in the ZIMRA documentation because disabling verification mutual trust is not implemented.
            #To check production version in consultation with ZIMRA
        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code
        print(jsonify(response.json()))
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON


@app.route('/verify/<deviceid>/<ack>/<dvser>', methods=['GET'])
def get_ver(deviceid,ack,dvser):

    url = "https://fdmsapitest.zimra.co.zw/Public/v1/" + deviceid +"/VerifyTaxpayerInformation"

    payload = json.dumps({
      "activationKey": ack,
      "deviceSerialNo": dvser
    })
    headers = {
      'Content-Type': 'application/json'
    }

    r = requests.request("POST", url, headers=headers, data=payload)


    if r == "[]":
        return jsonify([{'rsMessage':"We cannot reach the api."}])
    else:
        return jsonify(r.text)
        #return r


@app.route('/getStatus/<deviceid>/<model>/<version>', methods=['GET'])
def get_status(deviceid,model,version):
    client_cert = 'C:\\Users\\ADMIN\\client_cert.pem'  # Self-signed client certificate noot used in this situation
    client_key = 'C:\\Users\\ADMIN\\private_key.pem'    # Private key for the client certificate
    server_cert = 'C:\\Users\\ADMIN\\server_cert.pem'   #zimra server signed certificate

    url = "https://fdmsapitest.zimra.co.zw/Device/v1/"+ deviceid +"/GetStatus"  # API URL

    headers = {
        'accept': 'application/json',
        'DeviceModelName': model,
        'DeviceModelVersion': version
    }

    try:
        response = requests.get(
            url,
            headers=headers,
            cert=(server_cert, client_key),
            verify=False

        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code

        print(response.json())
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON


@app.route('/hash/<hashstring>/<sigstring>', methods=['GET'])
def get_hash(hashstring,sigstring):
    # Input string
    input_string = hashstring
    input_string1 = sigstring
    # Generate SHA-256 hash
    sha256_hash = hashlib.sha256(input_string.encode()).digest()
    sha256_hash1 = hashlib.sha256(input_string1.encode()).digest()
    # Base64 encode the hash
    base64_hash = base64.b64encode(sha256_hash).decode()
    base64_hash1 = base64.b64encode(sha256_hash1).decode()

    return jsonify({'hash': base64_hash, 'signature': base64_hash1})


@app.route('/openDay/<deviceid>/<model>/<version>/<dnumber>/<fdOpen>', methods=['GET'])
def get_open(deviceid,model,version,dnumber,fdOpen):
    client_cert = 'C:\\Users\\ADMIN\\client_cert.pem'  # Self-signed client certificate noot used in this situation
    client_key = 'C:\\Users\\ADMIN\\private_key.pem'    # Private key for the client certificate
    server_cert = 'C:\\Users\\ADMIN\\server_cert.pem'   #zimra server signed certificate

    url = "https://fdmsapitest.zimra.co.zw/Device/v1/"+ deviceid +"/OpenDay"  # API URL

    headers = {
        'accept': 'application/json',
        'DeviceModelName': model,
        'DeviceModelVersion': version,
        "Content-Type": "application/json"
    }
    dnumber = int(dnumber)
    if dnumber == 0:
        #payload = '{"openDayRequest":[{"fiscalDayOpened": "' + fdOpen + '"}]}'
        #dnumber = 101
        payload = {"fiscalDayNo": dnumber, "fiscalDayOpened":  fdOpen }
    else:
        payload = {"fiscalDayNo": dnumber, "fiscalDayOpened":  fdOpen }


    try:
        response = requests.post(
            url,
            headers=headers,
            json=payload,
            cert=(server_cert, client_key),
            verify=False

        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            #return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code
            print(response.json())
            return jsonify(response.json()), response.status_code
        print(response.json())
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        print(str(e))
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON





@app.route('/onlinerec/<deviceid>/<version>/<modname>/<refile>', methods=['GET'])
def onlinereceipt(deviceid,version,modname,refile):
    client_cert = 'C:\\Users\\ADMIN\\client_cert.pem'  # Self-signed client certificate noot used in this situation
    client_key = 'C:\\Users\\ADMIN\\private_key.pem'    # Private key for the client certificate
    server_cert = 'C:\\Users\\ADMIN\\server_cert.pem'   #zimra server signed certificate
    file_path = 'C:\\Users\\ADMIN\\OneDrive\\Documents\\Projects\\Progress\\Simacc\\reports\\receipts\\' + refile + '.json'

    url = "https://fdmsapitest.zimra.co.zw/Device/"+ version + "/" + deviceid +"/SubmitReceipt"

    headers = {
        "accept": "application/json",
        "DeviceModelName": modname,
        "DeviceModelVersion": version,
        "Content-Type": "application/json"
    }


    # Open and read the JSON file as a text string
    with open(file_path, 'r') as file:
        data = json.load(file)

    # Load private key from PEM file or string
    with open(client_key, "rb") as key_file:
        private_key = serialization.load_pem_private_key(
            key_file.read(),
            password=None
        )
    #payload = json_text

    rec = data["receipt"]
    receiptType = data["receipt"]["receiptType"]
    receiptCurrency = data["receipt"]["receiptCurrency"]
    receiptCounter = data["receipt"]["receiptCounter"]
    receiptGlobalNo = data["receipt"]["receiptGlobalNo"]
    invoiceNo = data["receipt"]["invoiceNo"]
    buyerData = data["receipt"]["buyerData"]
    receiptNotes = data["receipt"]["receiptNotes"]
    receiptDate = data["receipt"]["receiptDate"]
    receiptLinesTaxInclusive = data["receipt"]["receiptLinesTaxInclusive"]
    receiptLines = data["receipt"]["receiptLines"]
    receiptTaxes = data["receipt"]["receiptTaxes"]
    receiptPayments = data["receipt"]["receiptPayments"]
    receiptTotal = data["receipt"]["receiptTotal"]
    receiptPrintForm = data["receipt"]["receiptPrintForm"]
    #taxes = rec["receiptTaxes"]
    receipt_hash, receipt_signature = generate_signature_and_hash(rec, private_key)
    if receiptType == "FiscalTaxInvoice" or receiptType == "FiscalInvoice":
        payload =       {
              "receipt": {
                "receiptType": receiptType,
                "receiptCurrency": receiptCurrency,
                "receiptCounter": receiptCounter,
                "receiptGlobalNo": receiptGlobalNo,
                "invoiceNo": invoiceNo,
                "buyerData": buyerData,
                "receiptNotes": receiptNotes,
                "receiptDate": receiptDate,
                "receiptLinesTaxInclusive":receiptLinesTaxInclusive,
                "receiptLines": receiptLines,
                "receiptTaxes": receiptTaxes,
                "receiptPayments": receiptPayments,
                "receiptTotal": receiptTotal,
                "receiptPrintForm":receiptPrintForm,
                "receiptDeviceSignature": {
                  "hash": receipt_hash,
                  "signature": receipt_signature
                }
              }
            }

    if receiptType == "DEBITNOTE" or receiptType == "CREDITNOTE":
        creditDebitNote = data["receipt"]["creditDebitNote"]
        payload =  {
              "receipt": {
                "receiptType": receiptType,
                "receiptCurrency": receiptCurrency,
                "receiptCounter": receiptCounter,
                "receiptGlobalNo": receiptGlobalNo,
                "invoiceNo": invoiceNo,
                "buyerData": buyerData,
                "receiptNotes": receiptNotes,
                "receiptDate": receiptDate,
                "creditDebitNote": creditDebitNote,
                "receiptLinesTaxInclusive":receiptLinesTaxInclusive,
                "receiptLines": receiptLines,
                "receiptTaxes": receiptTaxes,
                "receiptPayments": receiptPayments,
                "receiptTotal": receiptTotal,
                "receiptPrintForm":receiptPrintForm,
                "receiptDeviceSignature": {
                  "hash": receipt_hash,
                  "signature": receipt_signature
                }
              }
            }

    try:
        response = requests.post(
            url,
            headers=headers,
            json=payload,
            cert=(server_cert, client_key),
            verify=False

        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code

        print(response.json())
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON

    #return jsonify(receipt_hash, receipt_signature)



@app.route('/offlinerec/<deviceid>/<version>/<modname>/<refile>', methods=['GET'])
def offlinereceipt(deviceid,version,modname,refile):
    client_cert = 'C:\\Users\\ADMIN\\client_cert.pem'  # Self-signed client certificate noot used in this situation
    client_key = 'C:\\Users\\ADMIN\\private_key.pem'    # Private key for the client certificate
    server_cert = 'C:\\Users\\ADMIN\\server_cert.pem'   #zimra server signed certificate
    file_path = 'C:\\Users\\ADMIN\\OneDrive\\Documents\\Projects\\Progress\\Simacc\\rec\\' + refile + '.json'


    url = "https://fdmsapitest.zimra.co.zw/Device/"+version+"/"+ deviceid + "/SubmitFile"


    headers = {
        "accept": "application/json",
        "DeviceModelName": modname,
        "DeviceModelVersion": version,
        "Content-Type": "application/json"
    }


    # Open and read the JSON file as a text string
    with open(file_path, 'r') as file:
        json_text = file.read()

    payload = json_text

    try:
        response = requests.post(
            url,
            headers=headers,
            json=payload,
            cert=(server_cert, client_key),
            verify=False

        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code

        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON


@app.route('/closeday/<deviceid>/<version>/<modname>/<refile>', methods=['GET'])
def closeday(deviceid, version, modname, refile):
    client_cert = 'C:\\Users\\ADMIN\\client_cert.pem'  # Self-signed client certificate noot used in this situation
    client_key = 'C:\\Users\\ADMIN\\private_key.pem'    # Private key for the client certificate
    server_cert = 'C:\\Users\\ADMIN\\server_cert.pem'   #zimra server signed certificate

    file_path = f'C:\\Users\\ADMIN\\OneDrive\\Documents\\Projects\\Progress\\Simacc\\reports\\receipts\\{refile}.json'
    url = f"https://fdmsapitest.zimra.co.zw/Device/{version}/{deviceid}/CloseDay"

    headers = {
        "accept": "application/json",
        "DeviceModelName": modname,
        "DeviceModelVersion": version,
        "Content-Type": "application/json"
    }


    with open(file_path, 'r') as file:
        data = json.load(file)


    fiscalDayNo_str = data["EOD"]["fiscalDayNo"]
    deviceID_upper = data["EOD"]["deviceID"].upper()
    fiscalDayDate = format_receipt_date(data["EOD"]["opened"])
    receiptCounter = data["EOD"]["receiptCounter"]
    serial = data["EOD"]["serial"]
    sales = data["EOD"]["Sales"]
    h = format_sales_eod(sales)
    """
    "receiptCounter": 10,
    "opened": "07/07/25",
    "closed": "22/07/25",
    "serial":
    """


    hashstring = str(deviceID_upper) + str(fiscalDayNo_str) + str(fiscalDayDate)

    hashstring1 = str(deviceID_upper) + str(fiscalDayNo_str) + str(fiscalDayDate) +"2025-07-07T22:21:14AUTO"

    # Compute SHA256 hash and base64 encode
    sha256_hash = hashlib.sha256(hashstring.encode('utf-8')).digest()
    base64_hash = base64.b64encode(sha256_hash).decode('utf-8')



    sha256_hash1 = hashlib.sha256(hashstring1.encode('utf-8')).digest()
    base64_hash1 = base64.b64encode(sha256_hash1).decode('utf-8')


    # Prepare fiscalDayDeviceSignature per spec
    fiscalDayDeviceSignature = {
        "hash": base64_hash,
        "signature": base64_hash1
    }

    # According to spec, omit zero-value counters or send empty list
    fiscalDayCounters = []

    # Prepare data to send with correct fields
    data = {
        "fiscalDayNo": int(fiscalDayNo_str),
        "fiscalDayCounters": fiscalDayCounters,
        "fiscalDayDeviceSignature": fiscalDayDeviceSignature,
        "receiptCounter": 0
    }
    return jsonify(data)
"""
    try:
        response = requests.post(
            url,
            headers=headers,
            json=data,
            cert=(server_cert, client_key),
            verify=False

        )

        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            #return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code
            print(response.json())
            return jsonify(response.json()), response.status_code

        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON
"""


if __name__ == '__main__':
    #app.run(debug=True)
    app.run(host="127.0.0.1", port=8090, debug=True)
