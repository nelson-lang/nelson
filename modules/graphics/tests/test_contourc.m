%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
Z = [1 2 3; 4 5 6; 7 8 9];
M = contourc(Z);
assert_isequal(size(M, 1), 2);
assert_istrue(size(M, 2) > 0);
%=============================================================================
col = 1;
while col <= size(M, 2)
  n = M(2, col);
  assert_istrue(n >= 2);
  assert_istrue(col + n <= size(M, 2));
  col = col + n + 1;
end
assert_isequal(col, size(M, 2) + 1);
%=============================================================================
M1 = contourc(Z, [5 5]);
levels = [];
col = 1;
while col <= size(M1, 2)
  levels = [levels, M1(1, col)];
  col = col + M1(2, col) + 1;
end
assert_isequal(unique(levels), 5);
%=============================================================================
M2 = contourc(Z, 3);
levels = [];
col = 1;
while col <= size(M2, 2)
  levels = [levels, M2(1, col)];
  col = col + M2(2, col) + 1;
end
assert_isequal(numel(unique(levels)), 3);
assert_istrue(min(levels) > min(Z(:)));
assert_istrue(max(levels) < max(Z(:)));
%=============================================================================
x = [10 20 30];
y = [-1 0 1];
M3 = contourc(x, y, Z, [5 5]);
col = 1;
while col <= size(M3, 2)
  n = M3(2, col);
  pts = M3(:, col + 1:col + n);
  assert_istrue(all(pts(1, :) >= min(x)));
  assert_istrue(all(pts(1, :) <= max(x)));
  assert_istrue(all(pts(2, :) >= min(y)));
  assert_istrue(all(pts(2, :) <= max(y)));
  col = col + n + 1;
end
%=============================================================================
M4 = contourc(flip(x), y, Z, [5 5]);
assert_isequal(size(M4, 1), 2);
assert_istrue(size(M4, 2) > 0);
%=============================================================================
assert_checkerror('contourc()', _('Wrong number of input arguments.'));
assert_checkerror('contourc(ones(3))', _('Z must contain at least two different finite values.'));
assert_checkerror('contourc([1 2; 3 4], [2 1])', _('Contour levels must be monotonically increasing.'));
assert_checkerror('contourc([1 2; 3 4], [NaN 1])', _('Contour levels must be finite values.'));
assert_checkerror('contourc([1 1], [1 2], [1 2; 3 4])', _('X vector values must be strictly monotonic.'));
assert_checkerror('contourc([1 2 3], [1 2], [1 2; 3 4], [2 2])', _('X vector length does not match Z.'));
%=============================================================================
