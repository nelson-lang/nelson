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
asserts.isequal(nargin('netcdf.open'), -1);
asserts.isequal(nargout('netcdf.open'), -1);
ok = false; try; netcdf.open(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
mode = bitor(netcdf.getConstant('NC_CLOBBER'), netcdf.getConstant('NC_NETCDF4'));
ncid = netcdf.create(filename, mode);
asserts.istrue(ncid >= 0);
netcdf.close(ncid);
ncid = netcdf.open(filename, netcdf.getConstant('NC_NOWRITE'));
asserts.istrue(ncid >= 0);
netcdf.close(ncid);
ok = false; try; netcdf.open([tempname(), '.nc'], netcdf.getConstant('NC_NOWRITE')); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



