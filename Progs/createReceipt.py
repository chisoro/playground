#api interface to create receipts.
#1. Non-fiscal receipts
#2. Online fiscal receipts
#3. Offline fiscal receipts
#Design/Written:....................Simbarashe chisoro
#Date:............................5 December 2024

from flask import Flask, render_template, request, url_for, redirect, session, jsonify
import requests
import dataFormat
from cryptography.hazmat.primitives import serialization


def generate_signature_and_hash(receipt, private_key):
    # Prepare fields
    fields = []
    fields.append(str(receipt['deviceID']))
    fields.append(receipt['receiptType'].upper())
    fields.append(receipt['receiptCurrency'].upper())
    fields.append(str(receipt['receiptGlobalNo']))
    fields.append(receipt['receiptDate'])  # already in ISO 8601 format
    fields.append(dataFormat.format_receipt_total(receipt['receiptTotal']))
    fields.append(dataFormat.format_receipt_taxes(receipt['receiptTaxes']))

    # Include previousReceiptHash if present (not first receipt in fiscal day)
    if receipt['receiptCounter'] != 1:
        previous_receipt_hash = receipt['pHash']
    else:
        previous_receipt_hash = None

    if previous_receipt_hash:
        fields.append(previous_receipt_hash)

    concatenated = dataFormat.concatenate_fields(fields)
    print(concatenated)
    receipt_hash = dataFormat.generate_hash(concatenated)
    signature = dataFormat.sign_concatenated_string(private_key, concatenated)

    return receipt_hash, signature


def receipt():
    r= "Hello"
    return r

def stu_receipt():
    r= "Hello"
    return r

def on_receipt(deviceid,version,modname,refile,client_cert,client_key,server_cert,link,receipt_path):
    file_path = f'{receipt_path}{refile}.json'
    url = f"{link}/Device/{version}/{deviceid}/SubmitReceipt"
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



def of_receipt(deviceid,version,modname,refile,client_cert,client_key,server_cert,link,receipt_path):
    file_path = f'{receipt_path}{refile}.json'
    url = f"{link}/Device/{version}/{deviceid}/SubmitFile"
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



def zx_report(deviceid,version,modname,refile,client_cert,client_key,server_cert,link,receipt_path):
    file_path = f'{receipt_path}{refile}.json'
    url = f"{link}/Device/{version}/{deviceid}/CloseDay"

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
    fiscalDayDate = dataFormat.format_receipt_date(data["EOD"]["opened"])
    receiptCounter = data["EOD"]["receiptCounter"]
    serial = data["EOD"]["serial"]
    sales = data["EOD"]["Sales"]
    h = dataFormat.format_sales_eod(sales)

    
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
