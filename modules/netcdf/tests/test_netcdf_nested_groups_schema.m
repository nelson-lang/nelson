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
timeDim = netcdf.defDim(ncid, 'time', netcdf.getConstant('NC_UNLIMITED'));
g1 = netcdf.defGrp(ncid, 'level1');
g2 = netcdf.defGrp(g1, 'level2');
g3 = netcdf.defGrp(g2, 'level3');
sampleDim = netcdf.defDim(g3, 'sample', 2);
varid = netcdf.defVar(g3, 'deep_values', netcdf.getConstant('NC_INT'), [timeDim sampleDim]);
netcdf.defVarChunking(g3, varid, netcdf.getConstant('NC_CHUNKED'), [3 2]);
netcdf.defVarDeflate(g3, varid, 1, 1, 2);
netcdf.defVarFill(g3, varid, 0, int32(-7));
netcdf.defVarFletcher32(g3, varid, 1);
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'title', 'nested schema');
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'root_i16', int16([-1 2]));
netcdf.putAtt(g1, netcdf.getConstant('NC_GLOBAL'), 'level1_single', single(1.25));
netcdf.putAtt(g2, netcdf.getConstant('NC_GLOBAL'), 'level2_u8', uint8([7 8]));
netcdf.putAtt(g3, netcdf.getConstant('NC_GLOBAL'), 'level3_text', 'deep');
netcdf.endDef(ncid);
netcdf.putVar(g3, varid, [0 0], [3 2], int32([1 2; 3 4; 5 6]));
netcdf.close(ncid);
info = ncinfo(source);
asserts.isequal(length(info.Groups), 1);
asserts.isequal(info.Groups(1).Name, 'level1');
asserts.isequal(info.Groups(1).Groups(1).Name, 'level2');
deep = info.Groups(1).Groups(1).Groups(1);
asserts.isequal(deep.Name, 'level3');
asserts.isequal(deep.Variables(1).Name, 'deep_values');
asserts.isequal(length(deep.Variables(1).Dimensions), 2);
asserts.isequal(deep.Variables(1).Dimensions(1).Name, 'time');
asserts.istrue(deep.Variables(1).Dimensions(1).Unlimited);
asserts.isequal(deep.Variables(1).Dimensions(2).Name, 'sample');
asserts.isequal(deep.Variables(1).Storage, netcdf.getConstant('NC_CHUNKED'));
asserts.isequal(deep.Variables(1).ChunkSize, [3 2]);
asserts.isequal(deep.Variables(1).DeflateLevel, 2);
asserts.istrue(deep.Variables(1).Shuffle);
asserts.istrue(deep.Variables(1).Fletcher32);
asserts.isequal(deep.Variables(1).FillValue, -7);
ncwriteschema(target, info);
ncid = netcdf.open(target, netcdf.getConstant('NC_NOWRITE'));
[xtype, len] = netcdf.inqAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'root_i16');
asserts.isequal(xtype, netcdf.getConstant('NC_SHORT'));
asserts.isequal(len, 2);
g1 = netcdf.inqNcid(ncid, 'level1');
g2 = netcdf.inqNcid(g1, 'level2');
g3 = netcdf.inqNcid(g2, 'level3');
[xtype, len] = netcdf.inqAtt(g1, netcdf.getConstant('NC_GLOBAL'), 'level1_single');
asserts.isequal(xtype, netcdf.getConstant('NC_FLOAT'));
asserts.isequal(len, 1);
[xtype, len] = netcdf.inqAtt(g2, netcdf.getConstant('NC_GLOBAL'), 'level2_u8');
asserts.isequal(xtype, netcdf.getConstant('NC_UBYTE'));
asserts.isequal(len, 2);
varid = netcdf.inqVarID(g3, 'deep_values');
[~, ~, dimids, ~] = netcdf.inqVar(g3, varid);
[dimName, ~] = netcdf.inqDim(g3, dimids(1));
asserts.isequal(dimName, 'time');
[storage, chunks] = netcdf.inqVarChunking(g3, varid);
asserts.isequal(storage, netcdf.getConstant('NC_CHUNKED'));
asserts.isequal(chunks, [3 2]);
[shuffle, deflate, level] = netcdf.inqVarDeflate(g3, varid);
asserts.isequal(shuffle, 1);
asserts.isequal(deflate, 1);
asserts.isequal(level, 2);
[noFill, fillValue] = netcdf.inqVarFill(g3, varid);
asserts.isequal(noFill, 0);
asserts.isequal(fillValue, -7);
asserts.isequal(netcdf.inqVarFletcher32(g3, varid), 1);
netcdf.close(ncid);
%=============================================================================
