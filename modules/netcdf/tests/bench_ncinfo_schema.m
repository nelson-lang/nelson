%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
dimid = netcdf.defDim(ncid, 'n', 1024);
for k = 1:12
  name = ['v', int2str(k)];
  varid = netcdf.defVar(ncid, name, netcdf.getConstant('NC_DOUBLE'), dimid);
  netcdf.putAtt(ncid, varid, 'units', ['unit', int2str(k)]);
end
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'title', 'schema benchmark');
netcdf.endDef(ncid);
netcdf.close(ncid);
tic();
for k = 1:50
  info = ncinfo(filename);
end
toc()
asserts.isequal(info.NumVariables, 12);
asserts.isequal(info.NumDimensions, 1);
asserts.isequal(info.Attributes(1).Name, 'title');
%=============================================================================
