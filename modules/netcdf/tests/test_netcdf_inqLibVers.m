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
asserts.isequal(nargin('netcdf.inqLibVers'), -1);
asserts.isequal(nargout('netcdf.inqLibVers'), -1);
ok = false; try; netcdf.inqLibVers(1); catch; ok = true; end; asserts.istrue(ok);
ver = netcdf.inqLibVers();
asserts.istrue(ischar(ver));
asserts.istrue(length(ver) > 0);
%=============================================================================




