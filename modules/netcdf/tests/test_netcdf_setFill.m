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
asserts.isequal(nargin('netcdf.setFill'), -1);
asserts.isequal(nargout('netcdf.setFill'), -1);
ok = false; try; netcdf.setFill(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
old = netcdf.setFill(ncid, netcdf.getConstant('NC_NOFILL'));
asserts.istrue(old == netcdf.getConstant('NC_FILL') || old == netcdf.getConstant('NC_NOFILL'));
netcdf.close(ncid);
ok = false; try; netcdf.setFill(-1, netcdf.getConstant('NC_FILL')); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



