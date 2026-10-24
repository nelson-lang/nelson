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
asserts.isequal(nargin('netcdf.inqDimIDs'), -1);
asserts.isequal(nargout('netcdf.inqDimIDs'), -1);
ok = false; try; netcdf.inqDimIDs(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
dimid = netcdf.defDim(ncid, 'n', 2);
ids = netcdf.inqDimIDs(ncid);
asserts.istrue(any(ids == dimid));
netcdf.close(ncid);
ok = false; try; netcdf.inqDimIDs(-1); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



