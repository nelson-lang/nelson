%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% the same anonymous function called with one and two outputs, in a loop
M = ones(2, 3);
f = @(k) size(M .* k);
for k = 1:5
  [r, c] = f(k);
  assert_isequal([r, c], [2 3]);
  s = f(k);
  assert_isequal(s, [2 3]);
  [r, c] = f(k);
  assert_isequal(c, 3);
end
%=============================================================================
g = @(x) max(x);
v = [3 9 1];
for k = 1:3
  [m, i] = g(v);
  assert_isequal([m, i], [9 2]);
  assert_isequal(g(v), 9);
end
%=============================================================================
