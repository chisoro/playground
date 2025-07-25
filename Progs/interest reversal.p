DEF VAR X AS INTEGER.
DEF VAR cum AS INTEGER.

X = 35.
CREATE dbBctr.
assign
    dbBctr.AMT[1] = 0
            dbBctr.AMT[2] = 0
            dbBctr.BDate = TODAY
            dbBctr.intBatch = X
           dbBctr.period = 202401
           dbBctr.UID  = "1".
cum = 1.
FOR EACH dbmtf WHERE dbmtf.prog = "dbint.p" NO-LOCK:
    FIND FIRST dbsgr WHERE  dbsgr.Sgrp = dbmtf.Sgrp NO-LOCK NO-ERROR.
  
        CREATE dbBatch.
           ASSIGN
                dbBatch.Age = 1
               dbBatch.Amt = dbmtf.Amt * -1
               dbBatch.dbAcc = dbmtf.dbAcc 
               dbBatch.Dept = dbmtf.Dept
               dbBatch.DESCRIP = dbmtf.DESCRIP
               dbBatch.Fund  = 0
               dbBatch.Iledger = dbsgr.IntLedger
               dbBatch.intBatch = X 
               dbBatch.Proj =  dbmtf.proj
               dbBatch.Ref = dbmtf.Ref
               dbBatch.Seq = cum 
               dbBatch.Sgrp = dbmtf.Sgrp
               dbBatch.trDate = dbmtf.trDate
               dbBatch.Vat = dbmtf.vat.
         DISPLAY dbmtf.dbAcc. PAUSE 0.
        cum = cum + 1.
   
    IF cum > 1000 THEN DO:
        X = X + 1.
        cum = 1.
        CREATE dbBctr.
        assign
            dbBctr.AMT[1] = 0
            dbBctr.AMT[2] = 0
            dbBctr.BDate = TODAY
            dbBctr.intBatch = X
           dbBctr.period = 202401
           dbBctr.UID  = "1".
      END.
END.
