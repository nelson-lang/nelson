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
