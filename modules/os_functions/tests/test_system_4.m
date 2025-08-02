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
  [s, w, d] = system(["PING -n 5 127.0.0.1>nul", "PING -n 7 127.0.0.1>nul", "PING -n 10 127.0.0.1>nul"], [1, 5, 3]);
  t = toc();
else
  tic();
  [s, w, d] = system(["sleep 4", "sleep 6", "sleep 9"], [1, 5, 3]);
  t = toc();
end
assert_istrue(t >= 5 && t < 10)
assert_istrue(d(1) >= 1000 && d(1) <= 5000)
assert_istrue(d(2) >= 5000 && d(2) <= 7000)
assert_istrue(d(3) >= 3000 && d(2) <= 10000)
if ispc()
  assert_isequal(s, [258, 258, 258])
else
  assert_isequal(s, [134,134, 134])
end
assert_isequal(w, ["ABORTED", "ABORTED", "ABORTED"])
%=============================================================================
