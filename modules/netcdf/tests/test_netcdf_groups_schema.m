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
source = [tempname(), '.nc'];
target = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(source, mode);
gid = netcdf.defGrp(ncid, 'science');
dimid = netcdf.defDim(gid, 'time', 4);
varid = netcdf.defVar(gid, 'signal', netcdf.getConstant('NC_DOUBLE'), dimid);
netcdf.defVarChunking(gid, varid, netcdf.getConstant('NC_CHUNKED'), 2);
netcdf.defVarDeflate(gid, varid, 1, 1, 1);
netcdf.defVarFill(gid, varid, 0, -99);
netcdf.defVarFletcher32(gid, varid, 1);
netcdf.putAtt(gid, netcdf.getConstant('NC_GLOBAL'), 'group_att', 'present');
netcdf.putAtt(gid, varid, 'units', 'count');
netcdf.endDef(ncid);
netcdf.putVar(gid, varid, [1 2 3 4]);
netcdf.close(ncid);
info = ncinfo(source);
asserts.isequal(length(info.Groups), 1);
asserts.isequal(info.Groups(1).Name, 'science');
asserts.isequal(info.Groups(1).Attributes(1).Name, 'group_att');
asserts.isequal(info.Groups(1).Variables(1).Name, 'signal');
hasUnits = false;
for k = 1:length(info.Groups(1).Variables(1).Attributes)
  hasUnits = hasUnits || strcmp(info.Groups(1).Variables(1).Attributes(k).Name, 'units');
end
asserts.istrue(hasUnits);
asserts.isequal(info.Groups(1).Variables(1).ChunkSize, 2);
asserts.isequal(info.Groups(1).Variables(1).DeflateLevel, 1);
asserts.istrue(info.Groups(1).Variables(1).Shuffle);
asserts.istrue(info.Groups(1).Variables(1).Fletcher32);
asserts.isequal(info.Groups(1).Variables(1).FillValue, -99);
ncwriteschema(target, info);
ncid = netcdf.open(target, netcdf.getConstant('NC_NOWRITE'));
gid = netcdf.inqNcid(ncid, 'science');
varid = netcdf.inqVarID(gid, 'signal');
asserts.isequal(netcdf.getAtt(gid, netcdf.getConstant('NC_GLOBAL'), 'group_att'), 'present');
asserts.isequal(netcdf.getAtt(gid, varid, 'units'), 'count');
[storage, chunks] = netcdf.inqVarChunking(gid, varid);
asserts.isequal(storage, netcdf.getConstant('NC_CHUNKED'));
asserts.isequal(chunks, 2);
[shuffle, deflate, level] = netcdf.inqVarDeflate(gid, varid);
asserts.isequal(shuffle, 1);
asserts.isequal(deflate, 1);
asserts.isequal(level, 1);
asserts.isequal(netcdf.inqVarFletcher32(gid, varid), 1);
[noFill, fillValue] = netcdf.inqVarFill(gid, varid);
asserts.isequal(noFill, 0);
asserts.isequal(fillValue, -99);
netcdf.close(ncid);
%=============================================================================