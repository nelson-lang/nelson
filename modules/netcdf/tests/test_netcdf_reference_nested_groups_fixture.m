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
filename = [nelsonroot(), '/modules/netcdf/tests/reference_nested_groups.nc'];
info = ncinfo(filename);
asserts.isequal(ncreadatt(filename, '/', 'title'), 'nested reference');
asserts.isequal(length(info.Groups), 1);
asserts.isequal(info.Groups(1).Name, 'outer');
asserts.isequal(info.Groups(1).Groups(1).Name, 'middle');
inner = info.Groups(1).Groups(1).Groups(1);
asserts.isequal(inner.Name, 'inner');
asserts.isequal(inner.Variables(1).Name, 'reference_values');
asserts.isequal(inner.Variables(1).Datatype, 'int32');
hasTime = false;
hasSample = false;
for k = 1:length(inner.Variables(1).Dimensions)
  hasTime = hasTime || (strcmp(inner.Variables(1).Dimensions(k).Name, 'time') && inner.Variables(1).Dimensions(k).Unlimited);
  hasSample = hasSample || strcmp(inner.Variables(1).Dimensions(k).Name, 'sample');
end
asserts.istrue(hasTime);
asserts.istrue(hasSample);
asserts.isequal(inner.Variables(1).Storage, netcdf.getConstant('NC_CHUNKED'));
asserts.isequal(sort(inner.Variables(1).ChunkSize), [2 3]);
asserts.isequal(inner.Variables(1).DeflateLevel, 1);
asserts.istrue(inner.Variables(1).Shuffle);
asserts.istrue(inner.Variables(1).Fletcher32);
asserts.isequal(inner.Variables(1).FillValue, -77);
ncid = netcdf.open(filename, netcdf.getConstant('NC_NOWRITE'));
[xtype, len] = netcdf.inqAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'root_short');
asserts.isequal(xtype, netcdf.getConstant('NC_SHORT'));
asserts.isequal(len, 2);
outer = netcdf.inqNcid(ncid, 'outer');
middle = netcdf.inqNcid(outer, 'middle');
innerId = netcdf.inqNcid(middle, 'inner');
[xtype, len] = netcdf.inqAtt(outer, netcdf.getConstant('NC_GLOBAL'), 'outer_single');
asserts.isequal(xtype, netcdf.getConstant('NC_FLOAT'));
asserts.isequal(len, 1);
[xtype, len] = netcdf.inqAtt(middle, netcdf.getConstant('NC_GLOBAL'), 'middle_byte');
asserts.isequal(xtype, netcdf.getConstant('NC_BYTE'));
asserts.isequal(len, 2);
varid = netcdf.inqVarID(innerId, 'reference_values');
[xtype, len] = netcdf.inqAtt(innerId, varid, 'scale_u16');
asserts.isequal(xtype, netcdf.getConstant('NC_USHORT'));
asserts.isequal(len, 2);
values = netcdf.getVar(innerId, varid);
asserts.isequal(sort(double(values(:))'), [10 20 30 40 50 60]);
netcdf.close(ncid);
%=============================================================================
