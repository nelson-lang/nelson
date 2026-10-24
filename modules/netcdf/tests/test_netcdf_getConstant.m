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
asserts.isequal(nargin('netcdf.getConstant'), -1);
asserts.isequal(nargout('netcdf.getConstant'), -1);
ok = false; try; netcdf.getConstant(); catch; ok = true; end; asserts.istrue(ok);
asserts.isequal(netcdf.getConstant('NC_GLOBAL'), -1);
asserts.istrue(netcdf.getConstant('NC_NOWRITE') >= 0);
ok = false; try; netcdf.getConstant('NC_DOES_NOT_EXIST'); catch; ok = true; end; asserts.istrue(ok);
% the unknown-constant condition carries a stable identifier
asserts.checkerror('netcdf.getConstant(''NC_DOES_NOT_EXIST'')', _('Unknown netCDF constant.'), 'Nelson:netcdf:unknownConstant');
%=============================================================================



