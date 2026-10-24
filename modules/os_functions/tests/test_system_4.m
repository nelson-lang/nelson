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
  [s, w, d] = system(["PING -n 3 127.0.0.1>nul", "PING -n 4 127.0.0.1>nul", "PING -n 5 127.0.0.1>nul"], [1, 3, 2]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 2", "sleep 3", "sleep 4"], [1, 3, 2]);
  t = toc();
end
asserts.istrue(t >= 3 && t < 8)
asserts.istrue(d(1) >= 1000 && d(1) <= 4000)
asserts.istrue(d(2) >= 3000 && d(2) <= 6000)
asserts.istrue(d(3) >= 2000 && d(3) <= 5000)
if ispc()
  asserts.isequal(s, [258, 258, 258])
else
  asserts.isequal(s, [134, 134, 134])
end
asserts.isequal(w, ["ABORTED", "ABORTED", "ABORTED"])
%=============================================================================
