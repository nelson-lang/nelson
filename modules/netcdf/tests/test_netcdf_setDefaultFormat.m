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
asserts.isequal(nargin('netcdf.setDefaultFormat'), -1);
asserts.isequal(nargout('netcdf.setDefaultFormat'), -1);
ok = false; try; netcdf.setDefaultFormat(); catch; ok = true; end; asserts.istrue(ok);
old = netcdf.setDefaultFormat(netcdf.getConstant('NC_FORMAT_CLASSIC'));
asserts.istrue(old >= 0);
netcdf.setDefaultFormat(old);
ok = false; try; netcdf.setDefaultFormat(-99); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



