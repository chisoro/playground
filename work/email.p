DEFINE VARIABLE objOutlook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookMsg AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookAttach AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE objOutlookRecip AS COM-HANDLE NO-UNDO.

CREATE "Outlook.Application" objOutlook.

objoutlookMsg = objOutlook:CreateItem(0).
objOutlookRecip = objOutlookMsg:Recipients:Add("siliotm@jobsim.co.zw").
objOutlookRecip:Type = 1.
objOutlookMsg:Subject = "Opendege Send Mail".
objOutlookMsg:Body = "This is the test of the year
    
    I love this game.
    
S. Mawire".

objOutlookMsg:Attachments:Add("p:\111.csv").
objOutlookRecip:Resolve.
objOutlookMsg:Send.
objoutlook:Quit().

RELEASE OBJECT objOutlook.
RELEASE OBJECT objOutlookMsg.
RELEASE OBJECT objOutlookRecip.
