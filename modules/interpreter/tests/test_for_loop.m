%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
k = 0;
for i = [1 2]', assert_isequal(i, [1 2]'), k = k + 1; end
assert_isequal(k, 1);
%=============================================================================
k = 0;
for i = [1 2]
  if k == 0
    assert_isequal(i, 1)
  else
    assert_isequal(i, 2)
  end
  k = k + 1;
end
assert_isequal(k, 2);
%=============================================================================
k = 0;
for i = {1;2}, assert_isequal(i, {1;2}), k = k + 1; end
assert_isequal(k, 1);
%=============================================================================
k = 0;
for i = {1 2}
  if k == 0
    assert_isequal(i, {1})
  else
    assert_isequal(i, {2})
  end
  k = k + 1;
end
assert_isequal(k, 2);
%=============================================================================
A = [1 2; 3 4];
B = repmat(A,[2 3 2]);
k = 0;
for i = B
  if k == 0
    assert_isequal(i, [1;3;1;3]);
  elseif k == 1
    assert_isequal(i, [2;4;2;4]);
  elseif k == 2
    assert_isequal(i, [1;3;1;3]);
  else
    k;
  end
  k = k + 1;
end
assert_isequal(k, 6);
%=============================================================================
% Sanity check: simple numeric summation (should trigger hoisted/index-caching path)
N = 1000;
s = 0;
for i = 1:N; s = s + i; end
assert_isequal(s, N*(N+1)/2);
%=============================================================================
% Reduction fast-path: larger N (double accumulator)
N = 100000;
s = 0;
for i = 1:N
  s = s + i;
end
assert_isequal(s, N*(N+1)/2);
% Reduction fast-path: integer accumulator (int32)
Ni = 10000; % keep sum within int32
s = int32(0);
for i = int32(1):int32(Ni)
  s = s + i;
end
assert_isequal(s, int32((Ni*(Ni+1))/2));
% Fallback correctness: function-call in body should still be correct
Nf = 2000;
identity = @(x) x;
s = 0;
for i = 1:Nf
  s = s + identity(i);
end
assert_isequal(s, Nf*(Nf+1)/2);
%=============================================================================
% Regression: ensure expression-only body with colon-range does not crash
Nexpr = 100;
counter = 0;
for i = 1:Nexpr, i, counter = counter + 1; end
assert_isequal(counter, Nexpr);

% Regression: minimal comma-form expression-only loop
for i = 1:10, i, end
% (no crash, no-op)

%=============================================================================
