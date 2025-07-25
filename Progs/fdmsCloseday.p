USING System.Net.Http.*. 
USING System.Environment.
/* Program.................fdmsCloseDay.p
   Notes:.................FDMs Close Day
   Author:.................S. Chisoro
   1. Create Z or X report. Z if fiscalDayStatus = FiscalDayClosed; X if fiscalDayStatus = FiscalDayOpened 
   2. CloseDay
  */

  
 session:DATA-ENTRY-RETURN = TRUE.

   DEFINE VARIABLE HttpClient AS CLASS System.Net.WebClient. 
   DEFINE VARIABLE webResponse AS LONGCHAR NO-UNDO. 
   DEF VAR txtWebResponse AS CHAR.
   DEF VAR txtUnWantedR AS CHAR EXTENT 8.

 DEF TEMP-TABLE tmpTax no-undo
    FIELD taxCode AS CHAR
    FIELD taxCur AS CHAR
    FIELD taxPercent AS DECIMAL
    FIELD taxID AS INTEGER
    FIELD taxAmount AS DECIMAL
    FIELD salesAmountWithTax AS DECIMAL.

  DEF TEMP-TABLE tmpTaxCur no-undo
   FIELD taxCur AS CHAR
   FIELD  taxMType AS CHAR
   FIELD taxAmount AS DECIMAL
    FIELD salesAmountWithTax AS DECIMAL.

DEF TEMP-TABLE tmpTaxCN NO-UNDO
    FIELD taxCode AS CHAR
    FIELD taxCur AS CHAR
    FIELD taxPercent AS DECIMAL
    FIELD taxID AS INTEGER
    FIELD taxAmount AS DECIMAL
    FIELD salesAmountWithTax AS DECIMAL.

DEF TEMP-TABLE tmpTaxDN NO-UNDO
    FIELD taxCode AS CHAR
    FIELD taxCur AS CHAR
    FIELD taxPercent AS DECIMAL
    FIELD taxID AS INTEGER
    FIELD taxAmount AS DECIMAL
    FIELD salesAmountWithTax AS DECIMAL.

DEF TEMP-TABLE tmpTaxQty
    FIELD recNu AS INTEGER
    FIELD taxCur AS CHAR
    FIELD descrip AS CHAR
    FIELD salesAmountWithTax AS DECIMAL.

DEFINE VARIABLE sellerName as character.
DEFINE VARIABLE sellerTradeName as character.
DEFINE VARIABLE sellervatNumber as character.
DEFINE VARIABLE sellerTIN as character.
DEFINE VARIABLE sellerPhone as character.
DEFINE VARIABLE sellerEmail as character.
DEFINE VARIABLE sellerProvince as character.
DEFINE VARIABLE sellerCity as character.
DEFINE VARIABLE sellerStreet as character.
DEFINE VARIABLE sellerHouseNumber as character.
DEFINE VARIABLE sellerDistrict as character.
DEF VAR txtqUrl AS CHAR.
DEF VAR varFDnumber LIKE fdmsRecCounter.fDNumber.
DEF VAR varDeviceID LIKE fdmsDevice.DeviceID.

DEF VAR cFile      AS CHAR.
DEFINE VARIABLE cJson AS CHARACTER NO-UNDO.
DEFINE VARIABLE bFirstLine AS LOGICAL NO-UNDO. 
DEFINE VARIABLE moneyTypeCode AS CHARACTER NO-UNDO.

DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHasClosed AS LOGICAL NO-UNDO.
DEFINE VARIABLE  countd AS INTEGER.
DEFINE VARIABLE tamount AS DECIMAL.
DEFINE VARIABLE varOpened LIKE fdmsRecCounter.recDate.
DEFINE VARIABLE varClosed LIKE fdmsRecCounter.recDate.
DEFINE VARIABL varSerial LIKE  fdmsDevice.serialNumber.


/********** MAIN LOGIC **********/
FIND FIRST simctr.
FIND FIRST simctr1 NO-LOCK NO-ERROR.
lHasClosed  = TRUE.
cErrorMessage = "".
FIND FIRST fdmsDevice WHERE fdmsDevice.dstat = "A" NO-LOCK NO-ERROR.
IF  NOT AVAILABLE fdmsDevice THEN DO:
    ASSIGN
    sellerName = SIMCTR.CONAME
    sellerTradeName = SIMCTR.CONAME
    sellervatNumber = SIMCTR.REGNO
    sellerTIN = SIMCTR.BPNO
    sellerPhone = SIMCTR.Phone
    sellerEmail =   ""
    sellerProvince = ""
    sellerCity = SIMCTR.Add3
    sellerStreet = SIMCTR.add2
    sellerHouseNumber = SIMCTR.Add1
    txtqUrl = "".
    
END.
ELSE IF AVAILABLE fdmsDevice THEN DO:
    ASSIGN
    sellerName = fdmsDevice.TNAME
    sellerTradeName = fdmsDevice.TNAME
    sellervatNumber = fdmsDevice.VATNO 
    sellerTIN = fdmsDevice.TIN
    sellerPhone = fdmsDevice.Phone
    sellerEmail =  fdmsDevice.contactemail
    sellerProvince = fdmsDevice.txtProvince
    sellerCity = fdmsDevice.txtCity
    sellerStreet = fdmsDevice.txtStreet
    sellerHouseNumber = fdmsDevice.txtHouse
    txtqUrl = fdmsDevice.qrUrl.
    FIND LAST fdmsRecCounter WHERE fdmsRecCounter.DeviceID = fdmsDevice.DeviceID NO-LOCK NO-ERROR.
    assign
        varFDNumber = fdmsRecCounter.FDNUmber
        varDeviceID = fdmsDevice.DeviceID
        varOpened = fdmsRecCounter.recDate
        varClosed = TODAY
        varSerial = fdmsDevice.serialNumber.

    /*check if all cashiers have done end of day*/
          FOR EACH dbRecCtr WHERE dbRecCtr.FDNumber = varFDNumber.
              IF (dbRecCtr.EOD = NO OR dbRecCtr.VALIDATE = NO)  THEN DO:
                   lHasClosed  = FALSE.
                  cErrorMessage = cErrorMessage + dbRecCtr.usercode + "~n".
              END.
          END.
          IF  lHasClosed  = FALSE THEN DO:
                 cErrorMessage = cErrorMessage + "  have not been Closed or Validated.".
                  MESSAGE cErrorMessage VIEW-AS ALERT-BOX.
                  APPLY 'CLOSE' TO THIS-PROCEDURE.
          END.
         ELSE IF lHasClosed  = TRUE THEN DO:
                        FOR EACH tblForex.
            
                                            FOR EACH dbrec WHERE dbRec.txtCur = tblForex.txtCur AND dbrec.FDNumber = varFDNumber.
                                                         FIND FIRST dbrCod WHERE dbRCod.rCode = DbRec.rCode NO-ERROR.
                                            
                                             FIND FIRST tmpTax WHERE tmpTax.taxID  = dbRCod.taxID AND tmpTax.taxCur = tblForex.txtCur NO-ERROR.
                                                 IF AVAILABLE tmpTax THEN DO:
                                                     ASSIGN
                                                         tmpTax.taxCode = dbRCod.taxCode
                                                         tmpTax.taxCur = tblForex.txtCur
                                                         tmpTax.taxPercent = dbRCod.vat%
                                                         tmpTax.taxAmount = tmpTax.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRec.Amount),2)
                                                         tmpTax.salesAmountWithTax = tmpTax.salesAmountWithTax + DbRec.Amount .
                                        
                                                 END.
                                                 ELSE IF NOT available tmpTax THEN DO:
                                                     CREATE tmpTax.
                                                     ASSIGN
                                                         tmpTax.TaxID = dbRCod.taxID
                                                         tmpTax.taxCode = dbRCod.taxCode
                                                         tmpTax.taxCur = tblForex.txtCur
                                                         tmpTax.taxPercent = dbRCod.vat%
                                                         tmpTax.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRec.Amount),2)
                                                         tmpTax.salesAmountWithTax = DbRec.Amount .
                                                 END.
                                            END.
                                           
                                            FOR EACH dbrecInv WHERE dbRecInv.txtCur = tblForex.txtCur AND dbrecInv.FDNumber = varFDNumber.
                                                         FIND FIRST dbrCod WHERE dbRCod.rCode = DbRecInv.rCode NO-ERROR.
                                            
                                             FIND FIRST tmpTax WHERE tmpTax.taxID  = dbRCod.taxID AND tmpTax.taxCur = tblForex.txtCur NO-ERROR.
                                                 IF AVAILABLE tmpTax THEN DO:
                                                     ASSIGN
                                                         tmpTax.taxCode = dbRCod.taxCode
                                                         tmpTax.taxCur = tblForex.txtCur
                                                         tmpTax.taxPercent = dbRCod.vat%
                                                         tmpTax.taxAmount = tmpTax.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRecInv.Amount),2)
                                                         tmpTax.salesAmountWithTax = tmpTax.salesAmountWithTax + DbRecInv.Amount .
                                        
                                                 END.
                                                 ELSE IF NOT available tmpTax THEN DO:
                                                     CREATE tmpTax.
                                                     ASSIGN
                                                         tmpTax.TaxID = dbRCod.taxID
                                                         tmpTax.taxCode = dbRCod.taxCode
                                                         tmpTax.taxCur = tblForex.txtCur
                                                         tmpTax.taxPercent = dbRCod.vat%
                                                         tmpTax.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRecInv.Amount),2)
                                                         tmpTax.salesAmountWithTax = DbRecInv.Amount .
                                                 END.
                                            END. /*dbRec and dbRecInv combined*/
                            
                                            /*dbRecCRN for credit notes*/
                                            FOR EACH dbRecCRN WHERE dbRecCRN.txtCur = tblForex.txtCur AND dbRecCRN.FDNumber = varFDNumber.
                                                         FIND FIRST dbrCod WHERE dbRCod.rCode = dbRecCRN.rCode NO-ERROR.
                                            
                                             FIND FIRST tmpTaxCN WHERE tmpTaxCN.taxID  = dbRCod.taxID AND tmpTaxCN.taxCur = tblForex.txtCur NO-ERROR.
                                                 IF AVAILABLE tmpTaxCN THEN DO:
                                                     ASSIGN
                                                         tmpTaxCN.taxCode = dbRCod.taxCode
                                                         tmpTaxCN.taxCur = tblForex.txtCur
                                                         tmpTaxCN.taxPercent = dbRCod.vat%
                                                         tmpTaxCN.taxAmount = tmpTaxCN.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  dbRecCRN.Amount),2)
                                                         tmpTaxCN.salesAmountWithTax = tmpTaxCN.salesAmountWithTax + dbRecCRN.Amount .
                                        
                                                 END.
                                                 ELSE IF NOT available tmpTaxCN THEN DO:
                                                     CREATE tmpTaxCN.
                                                     ASSIGN
                                                         tmpTaxCN.TaxID = dbRCod.taxID
                                                         tmpTaxCN.taxCode = dbRCod.taxCode
                                                         tmpTaxCN.taxCur = tblForex.txtCur
                                                         tmpTaxCN.taxPercent = dbRCod.vat%
                                                         tmpTaxCN.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  dbRecCRN.Amount),2)
                                                         tmpTaxCN.salesAmountWithTax = dbRecCRN.Amount .
                                                 END.
                                            END.
                            
                                            /*dbRecDRN for debit notes*/
                                            FOR EACH dbRecDRN WHERE dbRecDRN.txtCur = tblForex.txtCur AND dbRecDRN.FDNumber = varFDNumber.
                                                         FIND FIRST dbrCod WHERE dbRCod.rCode = dbRecDRN.rCode NO-ERROR.
                                            
                                             FIND FIRST tmpTaxDN WHERE tmpTaxDN.taxID  = dbRCod.taxID AND tmpTaxDN.taxCur = tblForex.txtCur NO-ERROR.
                                                 IF AVAILABLE tmpTaxDN THEN DO:
                                                     ASSIGN
                                                         tmpTaxDN.taxCode = dbRCod.taxCode
                                                         tmpTaxDN.taxCur = tblForex.txtCur
                                                         tmpTaxDN.taxPercent = dbRCod.vat%
                                                         tmpTaxDN.taxAmount = tmpTaxDN.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  dbRecDRN.Amount),2)
                                                         tmpTaxDN.salesAmountWithTax = tmpTaxDN.salesAmountWithTax + dbRecDRN.Amount .
                                        
                                                 END.
                                                 ELSE IF NOT available tmpTaxDN THEN DO:
                                                     CREATE tmpTaxDN.
                                                     ASSIGN
                                                         tmpTaxDN.TaxID = dbRCod.taxID
                                                         tmpTaxDN.taxCode = dbRCod.taxCode
                                                         tmpTaxDN.taxCur = tblForex.txtCur
                                                         tmpTaxDN.taxPercent = dbRCod.vat%
                                                         tmpTaxDN.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  dbRecDRN.Amount),2)
                                                         tmpTaxDN.salesAmountWithTax = dbRecDRN.Amount .
                                                 END.
                                            END.
                                       
                               END.
                               FOR EACH tblForex.
                                        
                                              FOR EACH dbrec WHERE dbRec.txtCur = tblForex.txtCur AND dbrec.FDNumber = varFDNumber.
                                                         FIND FIRST dbPay WHERE dbPay.Paytype = DbRec.PayType NO-ERROR.
                                                         FIND FIRST dbrCod WHERE dbRCod.rCode = DbRec.rCode NO-ERROR.
                            
                                                     IF trim(substring(dbPay.descrip,1,4)) = "CASH" THEN DO:
                                                         moneyTypeCode = "CASH".
                                                    END.
                                                    ELSE IF TRIM(substring(dbPay.descrip,1,4)) = "BTRF"  THEN DO:
                                                          moneyTypeCode = "BTRF".
                                                    END.
                                                    ELSE DO:
                                                        moneyTypeCode = "CARD".
                                                    END.
                            
                                                    FIND FIRST tmpTaxCur WHERE tmpTaxCur.taxCur = tblForex.txtCur  AND tmpTaxCur.taxMType = moneyTypeCode NO-ERROR.
                                                         IF AVAILABLE tmpTaxCur THEN DO:
                                                             ASSIGN
                                                                 tmpTaxCur.taxCur = tblForex.txtCur
                                                                 tmpTaxCur.taxMType = moneyTypeCode
                                                                 tmpTaxCur.taxAmount = tmpTaxCur.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRec.Amount),2)
                                                                 tmpTaxCur.salesAmountWithTax = tmpTaxCur.salesAmountWithTax + DbRec.Amount .
                                                
                                                         END.
                                                         ELSE IF NOT available tmpTaxCur THEN DO:
                                                             CREATE tmpTaxCur.
                                                             ASSIGN
                                                                 tmpTaxCur.taxCur = tblForex.txtCur
                                                                 tmpTaxCur.taxMType = moneyTypeCode
                                                                 tmpTaxCur.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRec.Amount),2)
                                                                 tmpTaxCur.salesAmountWithTax = DbRec.Amount .
                                                         END.
                                            END.
                            
                                            
                                            FOR EACH dbrecInv WHERE dbRecInv.txtCur = tblForex.txtCur AND dbrecInv.FDNumber = varFDNumber.
                                                         FIND FIRST dbPay WHERE dbPay.Paytype = DbRecInv.PayType NO-ERROR.
                                                         FIND FIRST dbrCod WHERE dbRCod.rCode = DbRecInv.rCode NO-ERROR.
                            
                                                         IF trim(substring(dbPay.descrip,1,4)) = "CASH" THEN DO:
                                                             moneyTypeCode = "CASH".
                                                        END.
                                                        ELSE IF TRIM(substring(dbPay.descrip,1,4)) = "BTRF"  THEN DO:
                                                              moneyTypeCode = "BTRF".
                                                        END.
                                                        ELSE DO:
                                                            moneyTypeCode = "CARD".
                                                        END.
                                    
                                                    FIND FIRST tmpTaxCur WHERE tmpTaxCur.taxCur = tblForex.txtCur  AND tmpTaxCur.taxMType = moneyTypeCode NO-ERROR.
                                                         IF AVAILABLE tmpTaxCur THEN DO:
                                                             ASSIGN
                                                                 tmpTaxCur.taxCur = tblForex.txtCur
                                                                 tmpTaxCur.taxMType = moneyTypeCode
                                                                 tmpTaxCur.taxAmount = tmpTaxCur.taxAmount + ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRecInv.Amount),2)
                                                                 tmpTaxCur.salesAmountWithTax = tmpTaxCur.salesAmountWithTax + DbRecInv.Amount .
                                                
                                                         END.
                                                         ELSE IF NOT available tmpTaxCur THEN DO:
                                                             CREATE tmpTaxCur.
                                                             ASSIGN
                                                                 tmpTaxCur.taxCur = tblForex.txtCur
                                                                 tmpTaxCur.taxMType = moneyTypeCode
                                                                 tmpTaxCur.taxAmount =   ROUND((( dbrCod.Vat% / (dbrCod.Vat% + 100))  *  DbRecInv.Amount),2)
                                                                 tmpTaxCur.salesAmountWithTax = DbRecInv.Amount.
                                                         END.
                                            END.
                            
                                /*Document quantities*/
                                           FOR EACH dbrec WHERE dbRec.txtCur = tblForex.txtCur AND dbrec.FDNumber = varFDNumber.
                                    
                                                    FIND FIRST tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur  AND tmpTaxQty.recNu = dbrec.recno AND tmpTaxQty.Descrip = "Invoice" NO-ERROR.
                                                         IF AVAILABLE tmpTaxQty THEN DO:
                                                             ASSIGN
                                                                tmpTaxQty.salesAmountWithTax = tmpTaxQty.salesAmountWithTax + DbRec.Amount .
                                                        END.
                                                        ELSE IF NOT available tmpTaxQty THEN DO:
                                                             CREATE tmpTaxQty.
                                                             ASSIGN
                                                                 tmpTaxQty.taxCur = tblForex.txtCur
                                                                 tmpTaxQty.descrip = "Invoice"
                                                                 tmpTaxQty.recNu = DbRec.recNo
                                                                 tmpTaxQty.salesAmountWithTax = DbRec.Amount.
                                                         END.
                                            END.
                            
                                           FOR EACH dbrecInv WHERE dbRecInv.txtCur = tblForex.txtCur AND dbrecInv.FDNumber = varFDNumber.
                                    
                                                    FIND FIRST tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur  AND tmpTaxQty.recNu = dbrecInv.recno AND tmpTaxQty.Descrip = "Invoice" NO-ERROR.
                                                         IF AVAILABLE tmpTaxQty THEN DO:
                                                             ASSIGN
                                                                tmpTaxQty.salesAmountWithTax = tmpTaxQty.salesAmountWithTax + DbRecInv.Amount .
                                                        END.
                                                         ELSE IF NOT available tmpTaxQty THEN DO:
                                                             CREATE tmpTaxQty.
                                                             ASSIGN
                                                                 tmpTaxQty.taxCur = tblForex.txtCur
                                                                 tmpTaxQty.descrip = "Invoice"
                                                                 tmpTaxQty.recNu = DbRecInv.recNo
                                                                 tmpTaxQty.salesAmountWithTax = DbRecInv.Amount.
                                                         END.
                                            END.
                                            FOR EACH dbrecCrN WHERE dbRecCRN.txtCur = tblForex.txtCur AND dbrecCRN.FDNumber = varFDNumber.
                                    
                                                    FIND FIRST tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur  AND tmpTaxQty.recNu = dbrecCRN.recno AND tmpTaxQty.Descrip = "Credit" NO-ERROR.
                                                         IF AVAILABLE tmpTaxQty THEN DO:
                                                             ASSIGN
                                                                tmpTaxQty.salesAmountWithTax = tmpTaxQty.salesAmountWithTax + DbRecCrN.Amount .
                                                        END.
                                                         ELSE IF NOT available tmpTaxQty THEN DO:
                                                             CREATE tmpTaxQty.
                                                             ASSIGN
                                                                 tmpTaxQty.taxCur = tblForex.txtCur
                                                                 tmpTaxQty.descrip = "Invoice"
                                                                 tmpTaxQty.recNu = DbRecCrN.recNo
                                                                 tmpTaxQty.salesAmountWithTax = DbRecCRN.Amount.
                                                         END.
                                            END.
                                            FOR EACH dbrecDrN WHERE dbRecDRN.txtCur = tblForex.txtCur AND dbrecDRN.FDNumber = varFDNumber.
                                    
                                                    FIND FIRST tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur  AND tmpTaxQty.recNu = dbrecDRN.recno AND tmpTaxQty.Descrip = "Dedit" NO-ERROR.
                                                         IF AVAILABLE tmpTaxQty THEN DO:
                                                             ASSIGN
                                                                tmpTaxQty.salesAmountWithTax = tmpTaxQty.salesAmountWithTax + DbRecDrN.Amount .
                                                        END.
                                                         ELSE IF NOT available tmpTaxQty THEN DO:
                                                             CREATE tmpTaxQty.
                                                             ASSIGN
                                                                 tmpTaxQty.taxCur = tblForex.txtCur
                                                                 tmpTaxQty.descrip = "Invoice"
                                                                 tmpTaxQty.recNu = DbRecCrN.recNo
                                                                 tmpTaxQty.salesAmountWithTax = DbRecDRN.Amount.
                                                         END.
                                            END.
                            
                            
                               END.
                            
                               
                               /*export json file*/
                               cFile  = simctr.repDir + "receipts\closeDay" + string(varFDNumber)  + ".json".
                               cJson = '~{' + CHR(10).  
                                cJson = cJson + '  "EOD": ~{' + CHR(10).
                                cJson = cJson + '    "fiscalDayNo": ' + string(varFDNumber) + ',' + CHR(10).  
                                cJson = cJson + '    "receiptCounter": '  + string(fdmsRecCounter.recNumber)  + ',' + CHR(10).
                                cJson = cJson + '    "opened": "'  + string(varOpened)  + '",' + CHR(10).
                                cJson = cJson + '    "closed": "'  + string(varClosed)  + '",' + CHR(10).
                                cJson = cJson + '    "deviceID": "'  + string(varDeviceID)  + '",' + CHR(10).
                                cJson = cJson + '    "serial": "'  + TRIM(varSerial)  + '",' + CHR(10).

                                cJson = cJson + '    "sellerData": ~{' + CHR(10).  
                                cJson = cJson + '      "sellerRegisterName": "' + trim(sellertradeName) + '",' + CHR(10). 
                                cJson = cJson + '      "sellerTradeName": "' + trim(sellertradeName) + '",' + CHR(10).  
                                cJson = cJson + '      "sellervatNumber": "' + trim(sellervatNumber) + '",' + CHR(10).  
                                cJson = cJson + '      "sellerTIN": "' + trim(sellerTIN) + '",' + CHR(10).
                                cJson = cJson + '      "sellerContacts": ~{' + CHR(10). 
                                cJson = cJson + '           "sellerHouse": "' + trim(sellerhouseNumber) + '",' + CHR(10). 
                                cJson = cJson + '           "sellerStreet": "' + trim(sellerStreet) + '",' + CHR(10). 
                                cJson = cJson + '           "sellerCity": "' + trim(sellerCity) + '",' + CHR(10).
                                cJson = cJson + '           "sellerDistrict": "' + trim(sellerDistrict) + '",' + CHR(10).
                                cJson = cJson + '           "sellerProvince": "' + trim(sellerProvince) + '",' + CHR(10).
                                cJson = cJson + '           "sellerphoneNo": "' + trim(sellerPhone) + '",' + CHR(10).  
                                cJson = cJson + '           "selleremail": "' + trim(sellerEmail) + '"' + CHR(10).  
                                cJson = cJson + '       }' + CHR(10).
                                cJson = cJson + '    },' + CHR(10).
                            
                                cJson = cJson + '    "Sales": [' + CHR(10). 
                               bFirstLine = TRUE.
                                FOR EACH tmpTax  NO-LOCK:
                                    IF NOT bFirstLine THEN   
                                        cJson = cJson + ',' + CHR(10).   
                                    ELSE  
                                        bFirstLine = FALSE.
                                   cJson = cJson + '      ~{' + CHR(10).
                                   cJson = cJson + '        "taxCode": "' + tmpTax.taxCode + '",' + CHR(10).
                                   cJson = cJson + '        "taxPercent": ' + STRING(tmpTax.taxPercent) + ',' + CHR(10).  
                                   cJson = cJson + '        "taxID": ' + STRING(tmpTax.taxID) + ',' + CHR(10). 
                                   cJson = cJson + '        "taxCur": "' + STRING(tmpTax.taxCur) + '",' + CHR(10). 
                                   cJson = cJson + '        "taxAmount": ' + STRING(tmpTax.taxAmount) + ',' + CHR(10).
                                   cJson = cJson + '        "salesAmountWithTax": ' + STRING(tmpTax.salesAmountWithTax)  + CHR(10). 
                                   cJson = cJson + '      }' + CHR(10).  
                            
                                END.
                                cJson = cJson + '    ],' + CHR(10).
                                cJson = cJson + '    "SalesByCurrency": [' + CHR(10). 
                                bFirstLine = TRUE.
                                 FOR EACH tmpTaxCur  NO-LOCK:
                                     IF NOT bFirstLine THEN   
                                         cJson = cJson + ',' + CHR(10).   
                                     ELSE  
                                         bFirstLine = FALSE.
                                    cJson = cJson + '      ~{' + CHR(10).
                                    cJson = cJson + '        "taxCur": "' + STRING(tmpTaxCur.taxCur) + '",' + CHR(10). 
                                    cJson = cJson + '        "taxMoney":"'  + tmpTaxCur.taxMType + '",' + CHR(10).
                                    cJson = cJson + '        "taxAmount": ' + STRING(tmpTaxCur.taxAmount) + ',' + CHR(10).
                                    cJson = cJson + '        "salesAmountWithTax": ' + STRING(tmpTaxCur.salesAmountWithTax)  + CHR(10). 
                                    cJson = cJson + '      }' + CHR(10).  
                                
                                 END.
                                 cJson = cJson + '    ],' + CHR(10).
                               cJson = cJson + '    "CreditNotes": [' + CHR(10). 
                               bFirstLine = TRUE.
                                FOR EACH tmpTaxCN  NO-LOCK:
                                    IF NOT bFirstLine THEN   
                                        cJson = cJson + ',' + CHR(10).   
                                    ELSE  
                                        bFirstLine = FALSE.
                                   cJson = cJson + '      ~{' + CHR(10).
                                   cJson = cJson + '        "taxCode": "' + tmpTaxCN.taxCode + '",' + CHR(10).
                                   cJson = cJson + '        "taxPercent": ' + STRING(tmpTaxCN.taxPercent) + ',' + CHR(10).  
                                   cJson = cJson + '        "taxID": ' + STRING(tmpTaxCN.taxID) + ',' + CHR(10). 
                                   cJson = cJson + '        "taxCur": "' + STRING(tmpTaxCN.taxCur) + '",' + CHR(10). 
                                   cJson = cJson + '        "taxAmount": ' + STRING(tmpTaxCN.taxAmount) + ',' + CHR(10).
                                   cJson = cJson + '        "salesAmountWithTax": ' + STRING(tmpTaxCN.salesAmountWithTax)  + CHR(10). 
                                   cJson = cJson + '      }' + CHR(10).  
                            
                                END.
                                cJson = cJson + '    ],' + CHR(10).

                                cJson = cJson + '    "DebitNotes": [' + CHR(10). 
                               bFirstLine = TRUE.
                                FOR EACH tmpTaxDN  NO-LOCK:
                                    IF NOT bFirstLine THEN   
                                        cJson = cJson + ',' + CHR(10).   
                                    ELSE  
                                        bFirstLine = FALSE.
                                   cJson = cJson + '      ~{' + CHR(10).
                                   cJson = cJson + '        "taxCode": "' + tmpTaxDN.taxCode + '",' + CHR(10).
                                   cJson = cJson + '        "taxPercent": ' + STRING(tmpTaxDN.taxPercent) + ',' + CHR(10).  
                                   cJson = cJson + '        "taxID": ' + STRING(tmpTaxDN.taxID) + ',' + CHR(10). 
                                   cJson = cJson + '        "taxCur": "' + STRING(tmpTaxDN.taxCur) + '",' + CHR(10). 
                                   cJson = cJson + '        "taxAmount": ' + STRING(tmpTaxDN.taxAmount) + ',' + CHR(10).
                                   cJson = cJson + '        "salesAmountWithTax": ' + STRING(tmpTaxDN.salesAmountWithTax)  + CHR(10). 
                                   cJson = cJson + '      }' + CHR(10).  
                            
                                END.
                                cJson = cJson + '    ],' + CHR(10).
                                cJson = cJson + '    "DayQuantities": [' + CHR(10). 
                                bFirstLine = TRUE.
                                FOR EACH tblForex.
                                    IF NOT bFirstLine THEN   
                                        cJson = cJson + ',' + CHR(10).   
                                    ELSE  
                                        bFirstLine = FALSE.
                                       countd = 0.
                                       tamount = 0.
                                       FOR EACH tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur AND tmpTaxQty.Descrip = "Invoice".
                                                countd = countd + 1.
                                                tamount = tamount + tmpTaxQty.salesAmountWithTax .
                                        END.
                                       cJson = cJson + '      ~{' + CHR(10).
                                       cJson = cJson + '        "taxCur": "' + tblForex.txtCur + '",' + CHR(10). 
                                       cJson = cJson + '        "Amount": ' + STRING(tamount) + ',' + CHR(10).
                                       cJson = cJson + '        "Quantity": ' + STRING(countd) + ',' + CHR(10).
                                       cJson = cJson + '        "Type": "Invoice"' + CHR(10).

                                       cJson = cJson + '      },' + CHR(10).  
                                       countd = 0.
                                       tamount = 0.
                                       FOR EACH tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur AND tmpTaxQty.Descrip = "Credit".
                                                countd = countd + 1.
                                                tamount = tamount + tmpTaxQty.salesAmountWithTax .
                                        END.
                                         cJson = cJson + '      ~{' + CHR(10).
                                       cJson = cJson + '        "taxCur": "' + tblForex.txtCur + '",' + CHR(10). 
                                       cJson = cJson + '        "Amount": ' + STRING(tamount) + ',' + CHR(10).
                                       cJson = cJson + '        "Quantity": ' + STRING(countd) + ',' + CHR(10).
                                       cJson = cJson + '        "Type": "Credit"' + CHR(10).

                                       cJson = cJson + '      },' + CHR(10).  
                                       countd = 0.
                                       tamount = 0.
                                       FOR EACH tmpTaxQty WHERE tmpTaxQty.taxCur = tblForex.txtCur AND tmpTaxQty.Descrip = "Debit".
                                                countd = countd + 1.
                                                tamount = tamount + tmpTaxQty.salesAmountWithTax .
                                        END.
                                         cJson = cJson + '      ~{' + CHR(10).
                                       cJson = cJson + '        "taxCur": "' + tblForex.txtCur + '",' + CHR(10). 
                                       cJson = cJson + '        "Amount": ' + STRING(tamount) + ',' + CHR(10).
                                       cJson = cJson + '        "Quantity": ' + STRING(countd) + ',' + CHR(10).
                                       cJson = cJson + '        "Type": "Debit"' + CHR(10).

                                       cJson = cJson + '      }' + CHR(10).  


                                END.
                                cJson = cJson + '    ]' + CHR(10).
                                cJson = cJson + '    }' + CHR(10).
                                cJson = cJson + '    }' + CHR(10).
                            
                            OUTPUT TO VALUE(cFile).  
                            PUT UNFORMATTED cJson.
                            OUTPUT CLOSE.
                            MESSAGE "File has been submitted susccessfully. The process is asynchronous, run get-status to check" VIEW-AS ALERT-BOX.
         END.
END.


   
 

