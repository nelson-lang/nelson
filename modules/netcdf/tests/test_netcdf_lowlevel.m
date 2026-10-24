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
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
dimid = netcdf.defDim(ncid, 'x', 4);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.endDef(ncid);
netcdf.putVar(ncid, varid, [1 2 3 4]);
netcdf.close(ncid);
ncid = netcdf.open(filename, netcdf.getConstant('NC_NOWRITE'));
varid = netcdf.inqVarID(ncid, 'x');
data = netcdf.getVar(ncid, varid);
netcdf.close(ncid);
asserts.isequal(data(:)', [1 2 3 4]);
%=============================================================================




