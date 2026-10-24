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
asserts.isequal(nargin('netcdf.getChunkCache'), -1);
asserts.isequal(nargout('netcdf.getChunkCache'), -1);
ok = false; try; netcdf.getChunkCache(1); catch; ok = true; end; asserts.istrue(ok);
[s, n, p] = netcdf.getChunkCache();
asserts.istrue(s > 0);
asserts.istrue(n > 0);
asserts.istrue(p >= 0);
%=============================================================================




