import pyodbc
#conn = pyodbc.connect(r'Driver={Progress OpenEdge 10.2 driver};HOST=localhost;PORT=19691;DB=db003;UID=Sysprogress;PWD=sysprogress;DIL=0')
conn = pyodbc.connect(r'Driver={Progress OpenEdge 10.2 driver};HOST=localhost;PORT=19691;DB=db003;UID=Sysprogress;PWD=sysprogress;DIL=0')
cursor = conn.cursor()
cursor.execute('select * from pub.dbcmf')

for row in cursor.fetchall():
    print (row)
