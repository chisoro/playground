CREATE dbRecCtr.
ASSIGN
    EOD = Yes
    RecDate = 07/13/23
    RecTotal = 0
    usercode = "19"
    VALIDATE = NO.

FOR EACH dbRecH WHERE opCode = "19":
    CREATE dbRec.
    ASSIGN
        dbRec.Account = dbRecH.Account 
        dbRec.Amount  = dbRecH.Amount 
        dbRec.contype = dbRecH.contype 
        dbRec.decRate = dbRecH.decRate 
        dbRec.Descrip = dbRecH.Descrip 
        dbRec.OpCode = dbRecH.OpCode 
        dbRec.Paytype = dbRecH.Paytype 
        dbRec.rCode = dbRecH.rCode 
        dbRec.RecDate = dbRecH.RecDate 
        dbRec.RecNo = dbRecH.RecNo 
        dbRec.RecStat = dbRecH.RecStat 
        dbRec.Ref = dbRecH.Ref 
        dbRec.SeqNo = dbRecH.SeqNo 
        dbRec.txtCur = dbRecH.txtCur.
    DELETE dbRecH.
END.
