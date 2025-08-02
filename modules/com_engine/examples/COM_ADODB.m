%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Database connection with ADODB.Connection COM interface
% Require a database engine installed on your PC
% Some example strings for common databases:
% Oracle - PROVIDER=OraOLEDB.Oracle; Data Source=MyOracleDB; User Id=myUsername; Password=myPassword;
% MS SQL Server -  PROVIDER=SQLOLEDB; Data Source=myServerAddress; Initial Catalog=myDataBase; User Id=myUsername; Password=myPassword;
% MySQL  - driver=MySQL ODBC 3.51 Driver; Server=myServerAddress; Database=myDataBase; UID=username; PWD=password;
% Access - PROVIDER=Microsoft.Jet.OLEDB.4.0; Data Source=C:\mydatabase.mdb;User Id=myUsername; Password=myPassword;
% Access 2007 - PROVIDER=Microsoft.ACE.OLEDB.12.0; Data Source=C:\mydatabase.accdb; Persist Security Info=False;
%=============================================================================
pConn = actxserver('ADODB.Connection')
commandInvoke = ['Provider=Microsoft.Jet.OLEDB.4.0;Data Source=', modulepath('com_engine'), '/examples/ADODB_test.mdb', ';User Id=admin;Password=;'];
set(pConn,'CursorLocation', 3)
invoke(pConn, 'Open', commandInvoke);
set(pConn,'CommandTimeout',60)
sqlQuery = 'select LASTNAME from TestTable';
invoke(pConn,'BeginTrans');
pData = invoke(pConn, 'Execute', sqlQuery);
invoke(pConn,'CommitTrans');
get(pData,'recordcount')
data = invoke(pData, 'getrows')
% another request
sqlQuery = 'select * from TestTable order by lastname, firstname';
invoke(pConn,'BeginTrans');
pData = invoke(pConn, 'Execute', sqlQuery);
invoke(pConn,'CommitTrans');
get(pData,'recordcount')
data = invoke(pData, 'getrows')
%=============================================================================
