/* Build the JSON string manually */  
DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.  
DEFINE VARIABLE receiptCurrency AS CHARACTER NO-UNDO.  
DEFINE VARIABLE receiptNotes AS CHARACTER NO-UNDO.  
DEFINE VARIABLE receiptDate AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOutput AS CHARACTER NO-UNDO.

DEF TEMP-TABLE receipts NO-UNDO
    FIELD receiptLineType AS CHAR
    FIELD receiptLineNo LIKE dbRec.seqno
    FIELD receiptLineName LIKE dbRec.Descrip
     FIELD receiptLinePrice LIKE dbRec.Amount
     FIELD receiptLineQuantity LIKE dbRec.Amount
     FIELD receiptLineTotal LIKE dbRec.Amount
     FIELD taxCode AS CHAR
     FIELD taxPercent LIKE fdmsTax.taxPerc
     FIELD taxID LIKE fdmsTax.taxID.

DEF TEMP-TABLE tmpTax no-undo
    FIELD taxCode AS CHAR
    FIELD taxPercent AS DECIMAL
    FIELD taxID AS INTEGER
    FIELD taxAmount AS DECIMAL
    FIELD salesAmountWithTax AS DECIMAL.

cFile  = "receipt.json".

FOR EACH dbRecH WHERE recno = 21641 AND opCode = "71" NO-LOCK:
    CREATE  receipts.
    ASSIGN
                  receiptLineType = "Sale"
                 receiptLineNo = dbRecH.seqno
                 receiptLineName = dbRecH.Descrip
                 receiptLinePrice = dbRecH.Amount
                 receiptLineQuantity = 1
                receiptLineTotal = dbRecH.Amount
                taxCode = "A"
                taxPercent = 15
                taxID = 1.
   
END.


cJson = '~{' + CHR(10).  
cJson = cJson + '  "receipt": ~{' + CHR(10).  
cJson = cJson + '    "receiptType": "FiscalInvoice",' + CHR(10).  
cJson = cJson + '    "receiptCurrency": "' + receiptCurrency + '",' + CHR(10).  
cJson = cJson + '    "receiptCounter": 0,' + CHR(10).  
cJson = cJson + '    "receiptGlobalNo": 0,' + CHR(10).  
cJson = cJson + '    "invoiceNo": "string",' + CHR(10).  
cJson = cJson + '    "buyerData": ~{' + CHR(10).  
cJson = cJson + '      "buyerRegisterName": "string",' + CHR(10).  
cJson = cJson + '      "buyerTradeName": "string",' + CHR(10).  
cJson = cJson + '      "vatNumber": "stringstr",' + CHR(10).  
cJson = cJson + '      "buyerTIN": "stringstri",' + CHR(10).  
cJson = cJson + '      "buyerContacts": ~{' + CHR(10).  
cJson = cJson + '        "phoneNo": "string",' + CHR(10).  
cJson = cJson + '        "email": "Ah9*S~sUVJVQwvJ}igP6=C*EJ66*#GiI~U%yyn08X~~+=*.dc%Woyx_hU|6wEy19sXvM-J}AobPiHCblZ0X2m4|EmL&Fz6T+Nhk5"' + CHR(10).  
cJson = cJson + '      },' + CHR(10).  
cJson = cJson + '      "buyerAddress": ~{' + CHR(10).  
cJson = cJson + '        "province": "string",' + CHR(10).  
cJson = cJson + '        "city": "string",' + CHR(10).  
cJson = cJson + '        "street": "string",' + CHR(10).  
cJson = cJson + '        "houseNo": "string",' + CHR(10).  
cJson = cJson + '        "district": "string"' + CHR(10).  
cJson = cJson + '      }' + CHR(10).  
cJson = cJson + '    },' + CHR(10).  
cJson = cJson + '    "receiptNotes": "' + receiptNotes + '",' + CHR(10).  
cJson = cJson + '    "receiptDate": "' + receiptDate + '",' + CHR(10).  
cJson = cJson + '    "creditDebitNote": ~{' + CHR(10).  
cJson = cJson + '      "receiptID": 0,' + CHR(10).  
cJson = cJson + '      "deviceID": 0,' + CHR(10).  
cJson = cJson + '      "receiptGlobalNo": 0,' + CHR(10).  
cJson = cJson + '      "fiscalDayNo": 0' + CHR(10).  
cJson = cJson + '    },' + CHR(10).  
cJson = cJson + '    "receiptLinesTaxInclusive": true,' + CHR(10).  
cJson = cJson + '    "receiptLines": [' + CHR(10).  

/* Add receipt lines using a FOR EACH loop */  
DEFINE VARIABLE bFirstLine AS LOGICAL NO-UNDO.  

bFirstLine = TRUE. /* A flag to handle commas before each new line entry */  

FOR EACH receipts NO-LOCK:  
    IF NOT bFirstLine THEN   
        cJson = cJson + ',' + CHR(10).  /* Add comma for subsequent items */  
    ELSE  
        bFirstLine = FALSE. /* After the first entry, we set the flag to FALSE */  

    cJson = cJson + '      ~{' + CHR(10).  
    cJson = cJson + '        "receiptLineType": "' + receipts.receiptLineType + '",' + CHR(10).  
    cJson = cJson + '        "receiptLineNo": ' + STRING(receipts.receiptLineNo) + ',' + CHR(10).  
    cJson = cJson + '        "receiptLineHSCode": "string",' + CHR(10).  
    cJson = cJson + '        "receiptLineName": "' + receipts.receiptLineName + '",' + CHR(10).  
    cJson = cJson + '        "receiptLinePrice": ' + STRING(receipts.receiptLinePrice) + ',' + CHR(10).  
    cJson = cJson + '        "receiptLineQuantity": ' + STRING(receipts.receiptLineQuantity) + ',' + CHR(10).  
    cJson = cJson + '        "receiptLineTotal": ' + STRING(receipts.receiptLineTotal) + ',' + CHR(10).  
    cJson = cJson + '        "taxCode": "' + receipts.taxCode + '",' + CHR(10).  
    cJson = cJson + '        "taxPercent": ' + STRING(receipts.taxPercent) + ',' + CHR(10).  
    cJson = cJson + '        "taxID": ' + STRING(receipts.taxID) + CHR(10).  
    cJson = cJson + '      }' + CHR(10).  
END.  

cJson = cJson + '    ],' + CHR(10).  
cJson = cJson + '    "receiptTaxes": [' + CHR(10).  
cJson = cJson + '      ~{' + CHR(10).  
cJson = cJson + '        "taxCode": "str",' + CHR(10).  
cJson = cJson + '        "taxPercent": 0,' + CHR(10).  
cJson = cJson + '        "taxID": 0,' + CHR(10).  
cJson = cJson + '        "taxAmount": 0,' + CHR(10).  
cJson = cJson + '        "salesAmountWithTax": 0' + CHR(10).  
cJson = cJson + '      }' + CHR(10).  
cJson = cJson + '    ],' + CHR(10).  
cJson = cJson + '    "receiptPayments": [' + CHR(10).  
cJson = cJson + '      ~{' + CHR(10).  
cJson = cJson + '        "moneyTypeCode": "Cash",' + CHR(10).  
cJson = cJson + '        "paymentAmount": 0' + CHR(10).  
cJson = cJson + '      }' + CHR(10).  
cJson = cJson + '    ],' + CHR(10).  
cJson = cJson + '    "receiptTotal": 0,' + CHR(10).  
cJson = cJson + '    "receiptPrintForm": "Receipt48",' + CHR(10).  
cJson = cJson + '    "receiptDeviceSignature": ~{' + CHR(10).  
cJson = cJson + '      "hash": "string",' + CHR(10).  
cJson = cJson + '      "signature": "string"' + CHR(10).  
cJson = cJson + '    }' + CHR(10).  
cJson = cJson + '  }' + CHR(10).  
cJson = cJson + '}' + CHR(10).  

/* Write the JSON to file */
OUTPUT TO VALUE(cFile).  
PUT UNFORMATTED cJson.  
OUTPUT CLOSE.  



MESSAGE "JSON file created: " cFile VIEW-AS ALERT-BOX.
