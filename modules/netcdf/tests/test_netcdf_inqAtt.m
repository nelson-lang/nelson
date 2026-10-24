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
asserts.isequal(nargin('netcdf.inqAtt'), -1);
asserts.isequal(nargout('netcdf.inqAtt'), -1);
ok = false; try; netcdf.inqAtt(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
dimid = netcdf.defDim(ncid, 'n', 2);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.putAtt(ncid, varid, 'units', 'meter');
asserts.isequal(netcdf.getAtt(ncid, varid, 'units'), 'meter');
[xtype, len] = netcdf.inqAtt(ncid, varid, 'units');
asserts.isequal(len, 5);
id = netcdf.inqAttID(ncid, varid, 'units');
name = netcdf.inqAttName(ncid, varid, id);
asserts.isequal(name, 'units');
netcdf.renameAtt(ncid, varid, 'units', 'unit');
asserts.isequal(netcdf.getAtt(ncid, varid, 'unit'), 'meter');
netcdf.copyAtt(ncid, varid, 'unit', ncid, netcdf.getConstant('NC_GLOBAL'));
asserts.isequal(netcdf.getAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'unit'), 'meter');
netcdf.delAtt(ncid, varid, 'unit');
netcdf.close(ncid);
ok = false; try; netcdf.getAtt(-1, varid, 'unit'); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



