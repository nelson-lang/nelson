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
asserts.isequal(nargin('netcdf.inqUnlimDims'), -1);
asserts.isequal(nargout('netcdf.inqUnlimDims'), -1);
ok = false; try; netcdf.inqUnlimDims(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
dimid = netcdf.defDim(ncid, 'time', netcdf.getConstant('NC_UNLIMITED'));
ids = netcdf.inqUnlimDims(ncid);
asserts.istrue(any(ids == dimid));
netcdf.close(ncid);
ok = false; try; netcdf.inqUnlimDims(-1); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



