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
import json
import hashlib
import base64
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.hazmat.primitives.asymmetric.utils import encode_dss_signature
from cryptography.hazmat.primitives.asymmetric import padding
from datetime import datetime
import generate_ZReport
import generate_invoiceA4
import generate_receipt48


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

    payload1 = {
        "receipt":rec,
        "receiptDeviceSignature": {
          "hash": receipt_hash,
          "signature": receipt_signature
        }
    }
    with open(file_path, 'w') as json_file:
        json.dump(payload1, json_file, indent=4)

    if receiptPrintForm == "Receipt48" and receiptType in ("FiscalTaxInvoice", "FiscalInvoice"):
        generate_receipt48.gen_report(receipt_path, refile)
    elif receiptPrintForm == "InvoiceA4" and receiptType in ("FiscalTaxInvoice", "FiscalInvoice"):
        generate_invoiceA4.gen_report(receipt_path, refile)
    elif receiptPrintForm == "Receipt48" and receiptType in ("CreditNote", "DebitNote"):
        generate_receipt48.gen_report48(receipt_path, refile)
    else:
        generate_invoiceA4.gen_reportA4(receipt_path, refile)



    '''try:
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
    '''
    return payload1


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

    with open(client_key, "rb") as key_file:
        private_key = serialization.load_pem_private_key(
            key_file.read(),
            password=None
        )

    fiscalDayNo_str = data["EOD"]["fiscalDayNo"]
    deviceID_upper = data["EOD"]["deviceID"].upper()
    fiscalDayDate = dataFormat.format_receipt_date1(data["EOD"]["opened"])
    #fiscalDayClosed = dataFormat.format_receipt_date(data["EOD"]["closed"])
    fiscalDayClosed = datetime.now().strftime('%Y-%m-%dT%H:%M:%S')

    reconciliationMode = "MANUAL".upper()
    receiptCounter = data["EOD"]["receiptCounter"]
    serial = data["EOD"]["serial"]
    sales = data["EOD"]["Sales"]
    sales_by_currency = data["EOD"]["SalesByCurrency"]
    sale_by_tax, sale_by_tax_by_tax = dataFormat.format_sales_eod(sales)
    balance_by_money_type = dataFormat.format_sales_mt(sales_by_currency)

    fields = []
    fields.append(str(deviceID_upper))
    fields.append(str(fiscalDayNo_str))
    fields.append(str(fiscalDayDate))
    fields.append(str(sale_by_tax))
    fields.append(str(sale_by_tax_by_tax))
    fields.append(str(balance_by_money_type))

    concatenated = dataFormat.concatenate_fields(fields)


    eod_hash = dataFormat.generate_hash(concatenated)


    fields = []
    fields.append(str(deviceID_upper))
    fields.append(str(fiscalDayNo_str))
    fields.append(str(fiscalDayDate))
    fields.append(str(fiscalDayClosed))
    fields.append(str(reconciliationMode))
    fields.append(str(sale_by_tax))
    fields.append(str(sale_by_tax_by_tax))
    fields.append(str(balance_by_money_type))

    concatenated = dataFormat.concatenate_fields(fields)

    print(concatenated)
    eod_hash1 = dataFormat.generate_hash(concatenated)
    eod_sign = dataFormat.sign_concatenated_string(private_key, eod_hash1)


    # Prepare fiscalDayDeviceSignature per spec
    fiscalDayDeviceSignature = {
        "hash": eod_hash,
        "signature": eod_sign
    }

    # According to spec, omit zero-value counters or send empty list
    fiscalDayCounters = []
    #hashstring = str(deviceID_upper) + str(fiscalDayNo_str) + str(fiscalDayDate)

    #hashstring1 = str(deviceID_upper) + str(fiscalDayNo_str) + str(fiscalDayDate) +"2025-07-07T22:21:14AUTO"

    # Compute SHA256 hash and base64 encode
    #sha256_hash = hashlib.sha256(hashstring.encode('utf-8')).digest()
    #base64_hash = base64.b64encode(sha256_hash).decode('utf-8')



    #sha256_hash1 = hashlib.sha256(hashstring1.encode('utf-8')).digest()
    #base64_hash1 = base64.b64encode(sha256_hash1).decode('utf-8')


    # Prepare fiscalDayDeviceSignature per spec
    #fiscalDayDeviceSignature = {
        #"hash": eod_hash,
        #"signature": base64_hash1
    #}

    # Prepare data to send with correct fields
    payload = {
        "fiscalDayNo": int(fiscalDayNo_str),
        "fiscalDayCounters": fiscalDayCounters,
        "fiscalDayDeviceSignature": fiscalDayDeviceSignature,
        "receiptCounter": receiptCounter
    }
    #return jsonify(data)


    try:
        response = requests.post(
            url,
            headers=headers,
            json=payload,
            cert=(server_cert, client_key),
            verify=False

        )

        #print(data)
        printjob = generate_ZReport.gen_report(receipt_path,refile)
        # Check if the response is empty or not and return accordingly
        if response.status_code != 200:
            return jsonify([{'rsMessage': "We cannot reach the API."}]), response.status_code

        print(response.json())
        return jsonify(response.json())  # Return JSON response

    except Exception as e:
        return jsonify([{'rsMessage': str(e)}]), 500  # Return error as JSON
