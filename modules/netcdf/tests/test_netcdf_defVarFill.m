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
asserts.isequal(nargin('netcdf.defVarFill'), -1);
asserts.isequal(nargout('netcdf.defVarFill'), -1);
ok = false; try; netcdf.defVarFill(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
dimid = netcdf.defDim(ncid, 'n', 4);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.defVarFill(ncid, varid, 0, -99);
[noFill, fillValue] = netcdf.inqVarFill(ncid, varid);
asserts.isequal(noFill, 0);
asserts.isequal(fillValue, -99);
netcdf.close(ncid);
ok = false; try; netcdf.inqVarFill(-1, 0); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



