%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  tic();
  [s, w, d] = system(["PING -n 5 127.0.0.1>nul", "PING -n 7 127.0.0.1>nul", "PING -n 10 127.0.0.1>nul"]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 4", "sleep 6", "sleep 9"]);
  t = toc();
end
assert_istrue(t >= 8 && t < 25)
assert_istrue(d(1) >= 3000 && d(1) <= 5000)
assert_istrue(d(2) >= 5000 && d(2) <= 7000)
assert_istrue(d(3) >= 8000 && d(2) <= 10000)
assert_isequal(s, zeros(1, 3))
%=============================================================================
