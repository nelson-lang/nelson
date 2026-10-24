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
asserts.isequal(nargin('netcdf.defVarDeflate'), -1);
asserts.isequal(nargout('netcdf.defVarDeflate'), -1);
ok = false; try; netcdf.defVarDeflate(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
dimid = netcdf.defDim(ncid, 'n', 10);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.defVarDeflate(ncid, varid, 1, 1, 1);
[shuffle, deflate, level] = netcdf.inqVarDeflate(ncid, varid);
asserts.isequal([shuffle deflate level], [1 1 1]);
netcdf.close(ncid);
ok = false; try; netcdf.inqVarDeflate(-1, 0); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



