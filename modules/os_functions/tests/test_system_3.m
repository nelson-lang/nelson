%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), 'Requires a native shell and process creation, unavailable in WebAssembly.');
if ispc()
  tic();
  [s, w, d] = system(["PING -n 3 127.0.0.1>nul", "PING -n 4 127.0.0.1>nul", "PING -n 5 127.0.0.1>nul"]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 2", "sleep 3", "sleep 4"]);
  t = toc();
end
asserts.istrue(t >= 3 && t < 12)
asserts.istrue(d(1) >= 1500 && d(1) <= 4000)
asserts.istrue(d(2) >= 2500 && d(2) <= 5000)
asserts.istrue(d(3) >= 3500 && d(3) <= 7000)
asserts.isequal(s, zeros(1, 3))
%=============================================================================
