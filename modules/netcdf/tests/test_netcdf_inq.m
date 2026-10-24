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
asserts.isequal(nargin('netcdf.inq'), -1);
asserts.isequal(nargout('netcdf.inq'), -1);
ok = false; try; netcdf.inq(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
dimid = netcdf.defDim(ncid, 'n', 2);
varid = netcdf.defVar(ncid, 'x', netcdf.getConstant('NC_DOUBLE'), dimid);
[ndims, nvars, natts, unlimdimid] = netcdf.inq(ncid);
asserts.isequal([ndims nvars natts], [1 1 0]);
netcdf.close(ncid);
ok = false; try; netcdf.inq(-1); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



