DEF SHARED VAR wsOp   LIKE dbRec.OpCode.
DEF SHARED VAR wsDate LIKE dbRec.RecDate.
DEF VAR wsRec  LIKE dbRec.RecNo.
DEF VAR wsMcno LIKE   munrctctr.mcno.

/*PROCEDURE Prom-Rec.ip: */
wsMcno = int(wsOp).
MESSAGE wsMcno " / " wsDate VIEW-AS ALERT-BOX.
DEF BUFFER bM FOR munrct.
/*DO ON ERROR UNDO, RETURN: */
    FIND FIRST munocf WHERE munocf.op-code = wsOp NO-ERROR.
        IF NOT AVAILABLE munocf THEN DO:
            MESSAGE "No Related Operator on the other System" 
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
    FIND FIRST munmmf WHERE munmmf.mcno = wsMcno NO-ERROR.
    IF NOT AVAILABLE munmmf THEN DO:
        MESSAGE "Invalid Machine number entered, please try again..." 
            VIEW-AS ALERT-BOX.
        BELL.
        PAUSE.
        NEXT.
    END.
    ELSE DO:
       IF  munmmf.current-op <> wsOp AND munmmf.current-op <> "" THEN DO:
           MESSAGE " Machine already in use by another operator -" munmmf.current-op.
           BELL.
           PAUSE.
           NEXT.
       END.
       IF  munocf.current-mmf NE wsMcno AND munocf.current-mmf NE 0 THEN DO:
           MESSAGE " You are currently logged into machine:" munocf.current-mmf.
           BELL.
           PAUSE.
           NEXT.
       END.
       IF NOT munmmf.eod THEN DO:
              MESSAGE " End of day not done, please do end of day first".
           BELL.
           PAUSE.
           NEXT.
       END.
        CREATE munrctctr.
        ASSIGN munrctctr.eff-date   = TODAY
               munrctctr.mcno       = wsMcno
               munrctctr.rec-date   = wsDate
               munrctctr.rec-status = "U"
               munrctctr.valid      = NO.

        FOR EACH  DBREC:
            DISPLAY   dbRec.contype .
            CREATE bM.
            ASSIGN  bM.acc      = dbRec.Account
                    bM.CODE     = dbRec.rCode
                    bM.amount   = dbRec.Amount
                    bM.eff-date = TODAY 
                    bM.mcno     = wsMcno
                    bM.opcode   = STRING(dbRec.OpCode)
                    bM.paytype  = "C" WHEN dbRec.Paytype >= 1 AND dbRec.Paytype <= 3 /* Cash */
                    bM.paytype  = "Q" WHEN dbRec.Paytype = 4 /* Ecocash */
                    bM.paytype  = "B" WHEN dbRec.Paytype >= 5 AND dbRec.Paytype <= 9 /* RTGS */
                    bM.paytype  = "P" WHEN dbRec.Paytype >= 10  AND dbRec.Paytype <= 14 /* Swipe */
                    bM.rec-date = dbRec.RecDate
                    bM.rec-status = "U"
                    /*bM.del-flag = NO  WHEN dbRec.RecStat = "" */
                    bM.del-flag = YES WHEN dbRec.RecStat = "C"
                    bM.recno    = dbRec.RecNo
                    bM.seq-no   = dbRec.SeqNo
                    /*bM.ref      = INT(dbRec.Ref) */   
                    bM.rec-name = dbRec.Descrip
                    wsRec       = dbRec.RecNo.
        END.
        FIND FIRST munrctctr WHERE munrctctr.eff-date = TODAY AND munrctctr.mcno = wsMcno
                           AND munrctctr.rec-date = wsDate EXCLUSIVE-LOCK.
        ASSIGN munrctctr.cash = 999999999.
        FIND CURRENT munmmf EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN munmmf.eod         = YES
               munmmf.current-op  = wsOp
               munmmf.last-rec-no = wsRec
               munmmf.rec-date    = wsDate.
    END.
/*END.
END PROCEDURE.*/
