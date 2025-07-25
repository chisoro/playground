DEF VAR wsb LIKE cbtrans.acb.
DEF VAR wsamt LIKE cbtrans.amount.
wsb = 1.
DO  WHILE wsb <> 0:
    UPDATE wsb wsamt.
    CREATE cbtrans.
    ASSIGN CBTrans.Acb = wsb
           CBTrans.Accper = 201812
           CBTrans.amount = wsAmt
           CBTrans.Descrip = "Reset for Auditted Data"
           CBTrans.OpCode  = "1"
           CBTrans.PoDate = TODAY 
           CBTrans.TransID = 202005121229
           CBTrans.TranType = 7
           CBTrans.trDate = 12/31/18
           CBTrans.UID    = "1"
           CBTrans.UID2   = "1"
           CBTrans.Ref    = "RESET".
        FIND FIRST cbkmf WHERE cbkmf.acb = wsb NO-ERROR.
    IF AVAILABLE cbkmf THEN
        cbkmf.bal = cbkmf.bal  + wsAmt.
END.
