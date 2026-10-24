%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
asserts.isequal(nargin('netcdf.inqVarIDs'), -1);
asserts.isequal(nargout('netcdf.inqVarIDs'), -1);
ok = false; try; netcdf.inqVarIDs(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
dimid = netcdf.defDim(ncid, 'n', 4);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
[name, xtype, dimids, natts] = netcdf.inqVar(ncid, varid);
asserts.isequal(name, 'x');
asserts.isequal(netcdf.inqVarID(ncid, 'x'), varid);
ids = netcdf.inqVarIDs(ncid);
asserts.istrue(any(ids == varid));
netcdf.renameVar(ncid, varid, 'y');
asserts.isequal(netcdf.inqVarID(ncid, 'y'), varid);
netcdf.endDef(ncid);
netcdf.putVar(ncid, varid, [1 2 3 4]);
data = netcdf.getVar(ncid, varid);
asserts.isequal(data(:)', [1 2 3 4]);
netcdf.close(ncid);
ok = false; try; netcdf.inqVarID(-1, 'x'); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



