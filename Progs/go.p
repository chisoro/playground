CONNECT -db dbacc -L 40000 -H localhost -S 19688  -U "SimAcc" -P Sim6771 -N TCP NO-ERROR.
CONNECT -db db001 -L 40000 -H localhost -S 19689  -U "SimAcc" -P Sim6771 -N TCP NO-ERROR.
CONNECT -db db002 -L 40000 -H localhost -S 19690  -U "SimAcc" -P Sim6771 -N TCP NO-ERROR.
CONNECT -db db003 -L 40000 -H localhost -S 19691  -U "SimAcc" -P Sim6771 -N TCP NO-ERROR.
CONNECT -db db004 -L 40000 -H localhost -S 19692  -U "SimAcc" -P Sim6771 -N TCP NO-ERROR.
CONNECT -db db005 -L 40000 -H localhost -S 19693  -U "SimAcc" -P Sim6771 -N TCP NO-ERROR.
RUN "simgo.p". 
    "QUIT".

