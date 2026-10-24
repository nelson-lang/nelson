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
asserts.isequal(nargin('netcdf.inqDimID'), -1);
asserts.isequal(nargout('netcdf.inqDimID'), -1);
ok = false; try; netcdf.inqDimID(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
dimid = netcdf.defDim(ncid, 'n', 4);
[name, len] = netcdf.inqDim(ncid, dimid);
asserts.isequal(name, 'n');
asserts.isequal(len, 4);
asserts.isequal(netcdf.inqDimID(ncid, 'n'), dimid);
netcdf.renameDim(ncid, dimid, 'm');
[name2, len2] = netcdf.inqDim(ncid, dimid);
asserts.isequal(name2, 'm');
asserts.isequal(len2, 4);
netcdf.close(ncid);
ok = false; try; netcdf.inqDimID(-1, 'n'); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



