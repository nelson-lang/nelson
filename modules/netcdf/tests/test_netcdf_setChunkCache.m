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
asserts.isequal(nargin('netcdf.setChunkCache'), -1);
asserts.isequal(nargout('netcdf.setChunkCache'), -1);
ok = false; try; netcdf.setChunkCache(); catch; ok = true; end; asserts.istrue(ok);
[s, n, p] = netcdf.getChunkCache();
netcdf.setChunkCache(s, n, p);
[s2, n2, p2] = netcdf.getChunkCache();
asserts.istrue(s2 > 0);
ok = false; try; netcdf.setChunkCache(-1, n, p); catch; ok = true; end; asserts.istrue(ok);
%=============================================================================



