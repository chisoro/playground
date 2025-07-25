/* CREATE Cshbatch  with bank-No + month 1,01 */
FIND cshtcf WHERE cshtcf.batch-no = ws-batch AND cshtcf.bank = wsBank
              AND cshtcf.company = g-company EXCLUSIVE-LOCK NO-ERROR .
IF AVAILABLE cshtcf AND cshtcf.batch-status = "P" THEN 
DO:
   MESSAGE "Batch already posted. Use another batch number.".
   RETURN NO-APPLY.
END.
IF NOT AVAILABLE cshtcf THEN 
DO:
  FIND cshbnk NO-LOCK WHERE cshbnk.bank = wsBank
                        AND cshbnk.company = g-company NO-LOCK NO-ERROR.
  IF NOT AVAILABLE cshbnk THEN 
  DO:
    MESSAGE "Bank Account " wsBank " is not on file.".
    RETURN NO-APPLY.
  END.
  ELSE IF AVAILABLE cshbnk  THEN
  DO:
    CREATE cshtcf.
    ASSIGN cshtcf.batch-tot   = 1
           cshtcf.batch-date = TODAY
           cshtcf.acc-pd     = wsPer
           cshtcf.descrip    = "Data Interfaced"
           cshtcf.bank       = wsBank.
  END.
END.

/* Create cshdcf */
CREATE cshdcf.
ASSIGN cshdcf.gl-acct   = ?
       cshdcf.batch-no  = cshtcf.batch-no
       cshdcf.company   = g-company
       cshdcf.dcf-alloc = 0
       cshdcf.dcf-seq     =
       cshdcf.tran-type   =
       cshdcf.dcf-gl-acct =
       cshdcf.dcf-date    =
       cshdcf.dcf-ref     =
       cshdcf.dcf-amount  =
       cshdcf.dcf-payee   = 
       cshdcf.dcf-narrat  =
       cshtcf.comp-tot    = cshtcf.comp-tot + 
RELEASE cshdcf.
RELEASE cshtcf.
RETURN.
