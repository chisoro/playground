import psycopg2
from sshtunnel import SSHTunnelForwarder
#con = psycopg2.connect(
    #database="cb_prod_maize",
    #user="schisoro",
    #password="loop&89@",
    #host="pgdbebs.cimmyt.org",
    #port= '22'
    #)
#cursor_obj.execute("SELECT * FROM  germplasm.package")
#result = cursor_obj.fetchall()
#print(result)

try:

    with SSHTunnelForwarder(
         ('pgdbebs.cimmyt.org', 22),
         ssh_private_key="c:/Users/SCHISORO/.ssh/id_rsa",
         ssh_username="schisoro",
         remote_bind_address=('localhost', 5432)) as server:

        server.start()
        print("server connected")

        params = {
            'database':'cb_prod_maize',
            'user':'schisoro',
            'password':'',
            'host':'localhost',
            'port':server.local_bind_port
            }

        conn = psycopg2.connect(**params)
        curs = conn.cursor()
        print("database connected")

except:
    print("Connection Failed")
