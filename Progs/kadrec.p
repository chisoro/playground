FOR EACH rec.munrct WHERE rec.munrct.rec-date = 02/28/23 
    AND rec.munrct.mcno = 86:
    CREATE munrct.
        ASSIGN
            dbRec.RecStat = "C" WHEN rec.munrct.del-flag = YES 
            db003.dbRec.Descrip = rec.munrct.rec-name
            db003.dbRec.RecNo = rec.munrct.recno 
            db003.dbRec.SeqNo = rec.munrct.seq-no
            db003.dbRec.RecDate = rec.munrct.rec-date 
            db003.dbRec.Account  = rec.munrct.acc 
            db003.dbRec.Amount  = rec.munrct.amount 
            db003.dbRec.rCode = rec.munrct.code 
            db003.dbrec.opcode = 75
            db003.dbRec.RecStat = rec.munrct.rec-status 
            db003.dbRec.contype 
            db003.dbRec.decRate = 1
            db003.dbRec.txtCur 0 "zwl"
            db003.dbRec.Ref = "PromImport"
            db003.dbRec.Paytype = 1 WHEN rec.munrct.paytype = "C"
            db003.dbRec.Paytype = 4 WHEN rec.munrct.paytype = "V" /*eco*/
            db003.dbRec.Paytype = 6 WHEN rec.munrct.paytype = "B" /* transfer */
            db003.dbRec.Paytype = 8 WHEN rec.munrct.paytype = "P". /*swipe*/
END.
CREATE dbrctctr.
ASSIGN db003.dbRecCtr.EOD = yes
       db003.dbRecCtr.RecDate = 02/09/23
       db003.dbRecCtr.usercode = 75.
