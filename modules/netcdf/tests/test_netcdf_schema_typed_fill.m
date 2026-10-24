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
dimid = netcdf.defDim(ncid, 'n', 3);
vDouble = netcdf.defVar(ncid, 'v_double', netcdf.getConstant('NC_DOUBLE'), dimid);
vSingle = netcdf.defVar(ncid, 'v_single', netcdf.getConstant('NC_FLOAT'), dimid);
vInt8 = netcdf.defVar(ncid, 'v_int8', netcdf.getConstant('NC_BYTE'), dimid);
vUint8 = netcdf.defVar(ncid, 'v_uint8', netcdf.getConstant('NC_UBYTE'), dimid);
vInt16 = netcdf.defVar(ncid, 'v_int16', netcdf.getConstant('NC_SHORT'), dimid);
vUint16 = netcdf.defVar(ncid, 'v_uint16', netcdf.getConstant('NC_USHORT'), dimid);
vInt32 = netcdf.defVar(ncid, 'v_int32', netcdf.getConstant('NC_INT'), dimid);
vUint32 = netcdf.defVar(ncid, 'v_uint32', netcdf.getConstant('NC_UINT'), dimid);
vInt64 = netcdf.defVar(ncid, 'v_int64', netcdf.getConstant('NC_INT64'), dimid);
vUint64 = netcdf.defVar(ncid, 'v_uint64', netcdf.getConstant('NC_UINT64'), dimid);
netcdf.defVarFill(ncid, vDouble, 0, -9.25);
netcdf.defVarFill(ncid, vSingle, 0, single(-1.5));
netcdf.defVarFill(ncid, vInt8, 0, int8(-2));
netcdf.defVarFill(ncid, vUint8, 0, uint8(250));
netcdf.defVarFill(ncid, vInt16, 0, int16(-1234));
netcdf.defVarFill(ncid, vUint16, 0, uint16(65000));
netcdf.defVarFill(ncid, vInt32, 0, int32(-123456));
netcdf.defVarFill(ncid, vUint32, 0, uint32(123456));
netcdf.defVarFill(ncid, vInt64, 0, int64(-123456789));
netcdf.defVarFill(ncid, vUint64, 0, uint64(123456789));
netcdf.close(ncid);
info = ncinfo(source);
asserts.isequal(info.Variables(1).Datatype, 'double');
asserts.isequal(info.Variables(2).Datatype, 'single');
asserts.isequal(info.Variables(3).Datatype, 'int8');
asserts.isequal(info.Variables(4).Datatype, 'uint8');
asserts.isequal(info.Variables(5).Datatype, 'int16');
asserts.isequal(info.Variables(6).Datatype, 'uint16');
asserts.isequal(info.Variables(7).Datatype, 'int32');
asserts.isequal(info.Variables(8).Datatype, 'uint32');
asserts.isequal(info.Variables(9).Datatype, 'int64');
asserts.isequal(info.Variables(10).Datatype, 'uint64');
asserts.isequal(double(info.Variables(1).FillValue), -9.25);
asserts.isequal(double(info.Variables(2).FillValue), -1.5);
asserts.isequal(double(info.Variables(3).FillValue), -2);
asserts.isequal(double(info.Variables(4).FillValue), 250);
asserts.isequal(double(info.Variables(5).FillValue), -1234);
asserts.isequal(double(info.Variables(6).FillValue), 65000);
asserts.isequal(double(info.Variables(7).FillValue), -123456);
asserts.isequal(double(info.Variables(8).FillValue), 123456);
asserts.isequal(double(info.Variables(9).FillValue), -123456789);
asserts.isequal(double(info.Variables(10).FillValue), 123456789);
ncwriteschema(target, info);
ncid = netcdf.open(target, netcdf.getConstant('NC_NOWRITE'));
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_double'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), -9.25);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_single'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), -1.5);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_int8'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), -2);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_uint8'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), 250);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_int16'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), -1234);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_uint16'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), 65000);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_int32'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), -123456);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_uint32'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), 123456);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_int64'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), -123456789);
[noFill, fillValue] = netcdf.inqVarFill(ncid, netcdf.inqVarID(ncid, 'v_uint64'));
asserts.isequal(noFill, 0);
asserts.isequal(double(fillValue), 123456789);
netcdf.close(ncid);
%=============================================================================
