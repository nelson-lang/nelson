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
asserts.isequal(netcdf.getConstant('NC_GLOBAL'), -1);
asserts.istrue(netcdf.getConstant('NC_NOWRITE') >= 0);
names = netcdf.getConstantNames();
asserts.istrue(any(strcmp(names, 'NC_GLOBAL')));
versionText = netcdf.inqLibVers();
asserts.istrue(ischar(versionText));
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
dimid = netcdf.defDim(ncid, 'n', 3);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.endDef(ncid);
netcdf.putVar(ncid, varid, [4 5 6]);
data = netcdf.getVar(ncid, varid);
asserts.isequal(data(:)', [4 5 6]);
netcdf.close(ncid);
%=============================================================================