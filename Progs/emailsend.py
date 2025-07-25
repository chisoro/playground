import win32com.client as win32
import pywin32_bootstrap
outlook = win32.Dispatch('outlook.application')
mail = outlook.CreateItem(0)
mail.To = 's.chisoroi@gmail.com'
mail.Subject = 'Test Message'
mail.Body = 'Please find attached your Statement'
mail.HTMLBody = '<h2>HTML Message body</h2>' #this field is optional

# To attach a file to the email (optional):
attachment  = "c:\simacc\Progs\content_pdf.pdf"
mail.Attachments.Add(attachment)

mail.Send()
