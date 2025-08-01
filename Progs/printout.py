import generate_invoiceA4

def re_print(filename,receipt_path):
    r = generate_invoiceA4.gen_report(receipt_path, filename)

    return r
