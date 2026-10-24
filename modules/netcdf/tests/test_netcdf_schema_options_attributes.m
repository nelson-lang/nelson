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
badTarget = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(source, mode);
nDim = netcdf.defDim(ncid, 'n', 4);
sDim = netcdf.defDim(ncid, 's', 5);
contigId = netcdf.defVar(ncid, 'contiguous_values', netcdf.getConstant('NC_DOUBLE'), nDim);
charId = netcdf.defVar(ncid, 'char_values', netcdf.getConstant('NC_CHAR'), sDim);
netcdf.defVarChunking(ncid, contigId, netcdf.getConstant('NC_CONTIGUOUS'), []);
netcdf.defVarFill(ncid, contigId, 1, -1);
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_i8', int8([-3 4]));
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_u16', uint16([5 6]));
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_single', single([1.5 2.5]));
netcdf.putAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_text', 'hello');
netcdf.putAtt(ncid, contigId, 'v_u32', uint32([10 20]));
netcdf.putAtt(ncid, charId, 'v_text', 'letters');
netcdf.endDef(ncid);
netcdf.putVar(ncid, charId, 'abcde');
netcdf.close(ncid);
info = ncinfo(source);
asserts.isequal(info.Variables(1).Name, 'contiguous_values');
asserts.isequal(info.Variables(1).Storage, netcdf.getConstant('NC_CONTIGUOUS'));
asserts.isequal(numel(info.Variables(1).ChunkSize), 0);
asserts.istrue(info.Variables(1).NoFill);
asserts.isequal(info.Variables(2).Datatype, 'char');
ncwriteschema(target, info);
ncid = netcdf.open(target, netcdf.getConstant('NC_NOWRITE'));
[xtype, len] = netcdf.inqAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_i8');
asserts.isequal(xtype, netcdf.getConstant('NC_BYTE'));
asserts.isequal(len, 2);
[xtype, len] = netcdf.inqAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_u16');
asserts.isequal(xtype, netcdf.getConstant('NC_USHORT'));
asserts.isequal(len, 2);
[xtype, len] = netcdf.inqAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_single');
asserts.isequal(xtype, netcdf.getConstant('NC_FLOAT'));
asserts.isequal(len, 2);
[xtype, len] = netcdf.inqAtt(ncid, netcdf.getConstant('NC_GLOBAL'), 'a_text');
asserts.isequal(xtype, netcdf.getConstant('NC_CHAR'));
asserts.isequal(len, 5);
contigId = netcdf.inqVarID(ncid, 'contiguous_values');
[storage, chunks] = netcdf.inqVarChunking(ncid, contigId);
asserts.isequal(storage, netcdf.getConstant('NC_CONTIGUOUS'));
asserts.isequal(chunks, 0);
[noFill, ~] = netcdf.inqVarFill(ncid, contigId);
asserts.isequal(noFill, 1);
[xtype, len] = netcdf.inqAtt(ncid, contigId, 'v_u32');
asserts.isequal(xtype, netcdf.getConstant('NC_UINT'));
asserts.isequal(len, 2);
charId = netcdf.inqVarID(ncid, 'char_values');
[xtype, len] = netcdf.inqAtt(ncid, charId, 'v_text');
asserts.isequal(xtype, netcdf.getConstant('NC_CHAR'));
asserts.isequal(len, 7);
netcdf.close(ncid);
bad = info;
bad.Variables(1).DimensionIDs = 99999;
ok = false;
try
  ncwriteschema(badTarget, bad);
catch
  ok = true;
end
asserts.istrue(ok);
%=============================================================================
