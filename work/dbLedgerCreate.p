DEF VAR wsAge LIKE dbBatch.Age.
DEF VAR    wsAmt LIKE dbBatch.Amt.
DEF VAR wsdbAcc LIKE    dbBatch.dbAcc. 
DEF VAR wsDept LIKE    dbBatch.Dept. 
DEF VAR wsDescrip LIKE dbBatch.DESCRIP. 
DEF VAR wsFund LIKE dbBatch.Fund. 
DEF VAR wsLedger LIKE dbBatch.Iledger. 
DEF VAR wsBatch LIKE dbBatch.intBatch.
DEF VAR wsBatch1 LIKE dbBatch.intBatch.
DEF VAR wsProj LIKE dbBatch.Proj.
DEF VAR wsRef LIKE dbBatch.Ref. 
DEF VAR wsSgrp LIKE dbBatch.Sgrp. 
DEF VAR wsDate LIKE dbBatch.trDate. 
DEF VAR wsDate1 LIKE dbBatch.trDate. 
DEF VAR wsVat LIKE dbBatch.Vat.
DEF VAR wsVat1 LIKE dbBatch.Vat.
DEF VAR wsSeq LIKE dbBatch.seq.
DEF VAR wsSeq1 LIKE dbBatch.seq.
DEF VAR wsTar LIKE dbtmf.tarif.
DEF VAR wsCAmt LIKE dbBatch.Amt.
DEF VAR wsRef1  LIKE dbBatch.Ref. 
DEF VAR wsAmt1 LIKE dbBatch.Amt.
DEF VAR wsCAmt1 LIKE dbBatch.Amt.

CREATE dbBCtr.
ASSIGN
    BDate  = Today
    intBatch = 200
    period = 202305
    UID = "1".

CREATE dbBCtr.
ASSIGN
    BDate  = Today
    intBatch = 201
    period = 202306
    UID = "1".
input from c:\simacc\work\accounttarifs.csv.
wsBatch = 200.
wsSeq = 1.
wsDate = TODAY.
wsBatch1 = 201.
wsSeq1 = 1.
wsDate1 = TODAY.
wsDescrip = "Bill Correction".

REPEAT:
    import delimiter ","  wsDbAcc  wsTar wsSgrp wsRef wsAmt wsCAmt wsRef1 wsAmt1 wsCAmt1.
    FIND FIRST dbtmf WHERE dbtmf.Sgrp  = wsSgrp AND   dbtmf.Tarif = wsTar.
    IF AVAILABLE dbtmf THEN DO:
               ASSIGN
                    wsDept = dbtmf.Dept
                    wsFund = dbtmf.fund
                    wsLedger = dbtmf.iledger
                     wsProj = dbtmf.proj.
               wsVat = - (wsAmt * (dbtmf.vat% / 100)).
               wsVat1 =  - (wsAmt1 * (dbtmf.vat% / 100)).
    END.
   IF wsAmt <> 0 THEN DO:
       CREATE dbBatch.
        ASSIGN
           dbBatch.Age  = 1
            dbBatch.Amt = - wsAmt
            dbBatch.dbAcc  = wsDbAcc
            dbBatch.Dept = wsDept
            dbBatch.DESCRIP = wsDescrip
            dbBatch.Fund = wsFund
            dbBatch.Iledger = wsLedger
            dbBatch.intBatch = wsBatch
            dbBatch.Proj = wsProj
            dbBatch.Ref = wsRef
            dbBatch.Seq = wsSeq
            dbBatch.Sgrp = wsSgrp
            dbBatch.trDate = wsDate
            dbBatch.Vat = wsVat.
         wsSeq = wsSeq + 1.
   END.

   IF wsAmt1 <> 0 THEN DO:
       CREATE dbBatch.
        ASSIGN
           dbBatch.Age  = 1
            dbBatch.Amt = - wsAmt1
            dbBatch.dbAcc  = wsDbAcc
            dbBatch.Dept = wsDept
            dbBatch.DESCRIP = wsDescrip
            dbBatch.Fund = wsFund
            dbBatch.Iledger = wsLedger
            dbBatch.intBatch = wsBatch1
            dbBatch.Proj = wsProj
            dbBatch.Ref = wsRef1
            dbBatch.Seq = wsSeq1
            dbBatch.Sgrp = wsSgrp
            dbBatch.trDate = wsDate1
            dbBatch.Vat = wsVat1.
         wsSeq1 = wsSeq1 + 1.
   END.
    
END.


