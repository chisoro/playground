from sshtunnel import SSHTunnelForwarder #Run pip install sshtunnel
from sqlalchemy.orm import sessionmaker #Run pip install sqlalchemy
from sqlalchemy import create_engine
from sqlalchemy.sql import text
import pandas as pd
import os
import csv
import tkinter
from tkinter import filedialog
root = tkinter.Tk()
root.withdraw() #use to hide tkinter window

currdir = os.getcwd()
tempfile = filedialog.askopenfilename(parent=root, initialdir=currdir, title='Please select file')

fo = "c:\\simacc\\reports\\germplasm.csv"
fo1 = "c:\\simacc\\reports\\packageexist.csv"
fo2 = "c:\\simacc\\reports\\originexist.csv"

df = pd.read_csv(tempfile)
seed_name = ""
stock_id = ""
seed_origin = ""

for i in range(len(df)):
    seed_name = seed_name + "'"+str(df.loc[i, "GERMPLASM_NAME"])+"',"
    stock_id = stock_id + "'"+str(df.loc[i, "PACKAGE_LABEL"])+"',"
    seed_origin = seed_origin + "'"+str(df.loc[i, "SEED_NAME"])+"',"

seed_name = seed_name.rstrip(seed_name[-1])
seed_origin = seed_origin.rstrip(seed_origin[-1])
stock_id = stock_id.rstrip(stock_id[-1])

with SSHTunnelForwarder(
    ('pgdbebs.cimmyt.org', 22), #Remote server IP and SSH port
    ssh_username = "schisoro",
    ssh_password = "Plli5e5sfxcRWnG7",
    remote_bind_address=('localhost', 5432)) as server: #PostgreSQL server IP and sever port on remote machine

    server.start() #start ssh sever
    print('Server connected via SSH')

    #connect to PostgreSQL
    local_port = str(server.local_bind_port)
    engine = create_engine('postgresql://schisoro:Plli5e5sfxcRWnG7@127.0.0.1:' + local_port +'/cb_prod_maize')

    Session = sessionmaker(bind=engine)
    session = Session()

    print('Database session created')

    stocks = session.execute(text("select distinct * from ("
    +"select distinct packages.package_code ,germ.designation,"
    +"germplasms.germplasm_id, germplasms.name_value, germplasms.germplasm_name_status, germplasms.germplasm_normalized_name,"
    +"	germ.germplasm_state,"
    +"	seeds.seed_name, seeds.id as seed_id, packages.package_label, packages.id as package_id, packages.program_id,prog.program_code , packages.creation_timestamp"
    +"	from germplasm.germplasm_name as germplasms"
    +"	join germplasm.germplasm as germ on germplasms.germplasm_id = germ.id"
    +"	join germplasm.seed as seeds on germplasms.germplasm_id = seeds.germplasm_id"
    +"	join germplasm.package as packages on seeds.id = packages.seed_id"
    +"	join tenant.program as prog on prog.id= seeds.program_id"
    +"	and packages.package_label  in ("+stock_id+")"
    +"  and germplasms.germplasm_name_status <>'deprecated'"
    +") as results"
    +" order by designation, seed_name"))
    rows1 = stocks.fetchall()
    df1 = pd.DataFrame(rows1)
    df1.to_csv(fo1, header = True, index = False)

    origin = session.execute(text("select distinct * from ("
    +"select distinct packages.package_code ,germ.designation,"
    +"germplasms.germplasm_id, germplasms.name_value, germplasms.germplasm_name_status, germplasms.germplasm_normalized_name,"
    +"	germ.germplasm_state,"
    +"	seeds.seed_name, seeds.id as seed_id, packages.package_label, packages.id as package_id, packages.program_id,prog.program_code , packages.creation_timestamp"
    +"	from germplasm.germplasm_name as germplasms"
    +"	join germplasm.germplasm as germ on germplasms.germplasm_id = germ.id"
    +"	join germplasm.seed as seeds on germplasms.germplasm_id = seeds.germplasm_id"
    +"	join germplasm.package as packages on seeds.id = packages.seed_id"
    +"	join tenant.program as prog on prog.id= seeds.program_id"
    +"	and seeds.seed_name in ("+seed_origin+")"
    +"  and germplasms.germplasm_name_status <>'deprecated'"
    +") as results"
    +" order by designation, seed_name"))
    rows2 = origin.fetchall()
    df2 = pd.DataFrame(rows2)
    df2.to_csv(fo2, header = True, index = False)

    germplasm = session.execute(text("select gn.name_value, g.germplasm_code, g.designation, gn.germplasm_id, g.designation, g.parentage, g.generation, g.germplasm_state, g.germplasm_name_type, g.germplasm_type"
     + " from germplasm.germplasm_name as gn join germplasm.germplasm as g on g.id = gn.germplasm_id"
	+ " where  gn.name_value  in ("+seed_name+") order by gn.name_value"))
    rows = germplasm.fetchall()
    df = pd.DataFrame(rows)
    df.to_csv(fo, header = True, index = False)


    os.startfile(fo)
    os.startfile(fo1)
    os.startfile(fo2)

    session.close()
