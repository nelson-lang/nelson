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
asserts.isequal(nargin('netcdf.inqVlen'), -1);
asserts.isequal(nargout('netcdf.inqVlen'), -1);
ok = false; try; netcdf.inqVlen(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
typeid = netcdf.defVlen(ncid, 'vlen_double', netcdf.getConstant('NC_DOUBLE'));
[name, datumSize, baseType] = netcdf.inqVlen(ncid, typeid);
asserts.isequal(name, 'vlen_double');
asserts.isequal(baseType, netcdf.getConstant('NC_DOUBLE'));
[userName, sizeValue, baseType2, nfields, classValue] = netcdf.inqUserType(ncid, typeid);
asserts.isequal(userName, 'vlen_double');
asserts.isequal(baseType2, netcdf.getConstant('NC_DOUBLE'));
netcdf.close(ncid);
ok = false; try; netcdf.inqVlen(-1, typeid); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



