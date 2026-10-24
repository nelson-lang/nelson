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
asserts.isequal(nargin('netcdf.inqFormat'), -1);
asserts.isequal(nargout('netcdf.inqFormat'), -1);
ok = false; try; netcdf.inqFormat(); catch; ok = true; end; asserts.istrue(ok);
filename = [tempname(), '.nc'];
ncid = netcdf.create(filename, netcdf.getConstant('NC_CLOBBER'));
fmt = netcdf.inqFormat(ncid);
asserts.istrue(fmt > 0);
netcdf.close(ncid);
ok = false; try; netcdf.inqFormat(-1); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



