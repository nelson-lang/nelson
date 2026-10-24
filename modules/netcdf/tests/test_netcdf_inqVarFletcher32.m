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
asserts.isequal(nargin('netcdf.inqVarFletcher32'), -1);
asserts.isequal(nargout('netcdf.inqVarFletcher32'), -1);
ok = false; try; netcdf.inqVarFletcher32(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
dimid = netcdf.defDim(ncid, 'n', 4);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.defVarFletcher32(ncid, varid, 1);
f = netcdf.inqVarFletcher32(ncid, varid);
asserts.isequal(f, 1);
netcdf.close(ncid);
ok = false; try; netcdf.inqVarFletcher32(-1, 0); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



