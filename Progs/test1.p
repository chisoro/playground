DEF VAR vdbAcc LIKE dbmtf.dbAcc.
DEF VAR  vAccper LIKE dbmtf.AccPer.
DEF VAR vAmt LIKE dbmtf.Amt.
DEF VAR vdecRate LIKE dbmtf.decRate.
DEF VAR vDept LIKE dbmtf.Dept.
DEF VAR vDESCRIP LIKE dbmtf.Descrip.
DEF VAR vprog LIKE dbmtf.prog.
DEF VAR vproj LIKE dbmtf.proj.
DEF VAR vRef LIKE dbmtf.Ref.
DEF VAR vSgrp LIKE dbmtf.Sgrp.
DEF VAR vTarif LIKE dbmtf.Tarif.
DEF VAR vtrDate LIKE dbmtf.trDate.
DEF VAR vtxtCur LIKE dbmtf.txtCur.
DEF VAR vtype LIKE dbmtf.TYPE.
DEF VAR vVat LIKE dbmtf.Vat.
DEF VAR vFund LIKE dbhtf.Fund.
DEF VAR vILedger LIKE dbhtf.ILedger. 
DEF VAR vPoDate LIKE dbhtf.PoDate.
DEF VAR vSeq LIKE dbhtf.Seq.
DEF VAR vTransID LIKE dbhtf.TransID.
DEF VAR vUID LIKE dbhtf.UID. 
DEF VAR vUID2 LIKE dbhtf.UID2.
vAmt = 0.
FOR EACH dbmtf WHERE dbAcc = 22061930 AND Tarif = 18:
    vAmt = vAmt + Amt.
    ASSIGN
       vAccper = Accper 
        vdbAcc = dbAcc 
        vdecRate = decRate 
        vDept = Dept 
        vDescrip = DESCRIP 
        vProg = prog 
        vProj = proj 
        vRef = Ref 
        vSgrp = Sgrp 
        vTarif = Tarif 
       vTrDate = trDate 
       vtxtCur = txtCur 
        vType = type 
        vVat = Vat.
    DELETE dbmtf.
END.
FOR EACH dbhtf WHERE dbAcc = 22061930 AND Tarif = 18 AND accPer = 202312:
    assign
        vFund = Fund 
        vILedger = ILedger 
        vPoDate = PoDate 
        vSeq = Seq
        vTransID = TransID 
        vUID = UID 
        vUID2 =UID2.
    
    DELETE dbhtf.
END.

CREATE dbmtf.
ASSIGN
     dbmtf.Accper = vAccper 
     dbmtf.dbAcc = vdbAcc 
     dbmtf.decRate = vdecRate 
     dbmtf.Dept = vDept 
     dbmtf.Descrip = vDESCRIP 
     dbmtf.Prog = vprog 
     dbmtf.Proj = vproj 
     dbmtf.Ref = vRef 
     dbmtf.Sgrp = vSgrp 
     dbmtf.Tarif = vTarif 
     dbmtf.TrDate = vtrDate 
     dbmtf.txtCur = vtxtCur 
     dbmtf.Type = vtype 
     dbmtf.Vat = vVat
      dbmtf.Amt = vAmt.

CREATE dbhtf.
ASSIGN
    dbhtf.Accper = vAccper 
    dbhtf.dbAcc = vdbAcc 
    dbhtf.decRate = vdecRate 
    dbhtf.Dept = vDept 
    dbhtf.Descrip = vDESCRIP 
    dbhtf.Prog = vprog 
    dbhtf.Proj = vproj 
    dbhtf.Ref = vRef 
    dbhtf.Sgrp = vSgrp 
    dbhtf.Tarif = vTarif 
    dbhtf.TrDate = vtrDate 
    dbhtf.txtCur = vtxtCur 
    dbhtf.Type = vtype 
    dbhtf.Vat = vVat
     dbhtf.Amt = vAmt
    dbhtf.Fund = vFund 
    dbhtf.ILedger = vILedger 
        dbhtf.PoDate = vPoDate 
        dbhtf.Seq = vSeq
        dbhtf.TransID = vTransID 
        dbhtf.UID = vUID 
        dbhtf.UID2 = vUID2.
DISPLAY vSgrp vTarif vDescrip vAmt vVat.
