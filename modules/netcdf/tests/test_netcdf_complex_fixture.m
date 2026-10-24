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
filename = [nelsonroot(), '/modules/netcdf/tests/reference_complex.nc'];
info = ncinfo(filename);
asserts.isequal(ncreadatt(filename, '/', 'title'), 'complex source');
asserts.isequal(length(info.Groups), 1);
asserts.isequal(info.Groups(1).Name, 'science');
asserts.isequal(info.Dimensions(1).Name, 'time');
asserts.istrue(info.Dimensions(1).Unlimited);
asserts.isequal(info.Variables(1).Name, 'series');
asserts.isequal(info.Variables(1).ChunkSize, 2);
asserts.isequal(info.Variables(1).DeflateLevel, 1);
asserts.istrue(info.Variables(1).Shuffle);
asserts.istrue(info.Variables(1).Fletcher32);
asserts.isequal(info.Variables(1).FillValue, -999);
ncid = netcdf.open(filename, netcdf.getConstant('NC_NOWRITE'));
seriesId = netcdf.inqVarID(ncid, 'series');
series = netcdf.getVar(ncid, seriesId);
asserts.isequal(series(:)', [11 12 13 14]);
gid = netcdf.inqNcid(ncid, 'science');
int16Id = netcdf.inqVarID(gid, 'int16_values');
uint8Id = netcdf.inqVarID(gid, 'uint8_values');
singleId = netcdf.inqVarID(gid, 'single_values');
asserts.isequal(double(netcdf.getVar(gid, int16Id)(:)'), [100 200 300]);
asserts.isequal(double(netcdf.getVar(gid, uint8Id)(:)'), [1 2 3]);
asserts.isequal(double(netcdf.getVar(gid, singleId)(:)'), [1.5 2.5 3.5]);
asserts.isequal(netcdf.getAtt(gid, netcdf.getConstant('NC_GLOBAL'), 'group_att'), 'present');
asserts.isequal(netcdf.getAtt(gid, int16Id, 'units'), 'count');
netcdf.close(ncid);
%=============================================================================