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
asserts.isequal(size(M, 1), 2);
asserts.istrue(size(M, 2) > 0);
%=============================================================================
col = 1;
while col <= size(M, 2)
  n = M(2, col);
  asserts.istrue(n >= 2);
  asserts.istrue(col + n <= size(M, 2));
  col = col + n + 1;
end
asserts.isequal(col, size(M, 2) + 1);
%=============================================================================
M1 = contourc(Z, [5 5]);
levels = [];
col = 1;
while col <= size(M1, 2)
  levels = [levels, M1(1, col)];
  col = col + M1(2, col) + 1;
end
asserts.isequal(unique(levels), 5);
%=============================================================================
M2 = contourc(Z, 3);
levels = [];
col = 1;
while col <= size(M2, 2)
  levels = [levels, M2(1, col)];
  col = col + M2(2, col) + 1;
end
asserts.isequal(numel(unique(levels)), 3);
asserts.istrue(min(levels) > min(Z(:)));
asserts.istrue(max(levels) < max(Z(:)));
%=============================================================================
x = [10 20 30];
y = [-1 0 1];
M3 = contourc(x, y, Z, [5 5]);
col = 1;
while col <= size(M3, 2)
  n = M3(2, col);
  pts = M3(:, col + 1:col + n);
  asserts.istrue(all(pts(1, :) >= min(x)));
  asserts.istrue(all(pts(1, :) <= max(x)));
  asserts.istrue(all(pts(2, :) >= min(y)));
  asserts.istrue(all(pts(2, :) <= max(y)));
  col = col + n + 1;
end
%=============================================================================
M4 = contourc(flip(x), y, Z, [5 5]);
asserts.isequal(size(M4, 1), 2);
asserts.istrue(size(M4, 2) > 0);
%=============================================================================
% Data with no relief carries no contour line, which contourc reports as an
% empty matrix rather than as a refusal, and without a word.
lastwarn('', '');
M5 = contourc(ones(3));
[msg, id] = lastwarn();
asserts.isequal(id, '');
asserts.isequal(size(M5), [2 0]);
asserts.istrue(isempty(M5));
%=============================================================================
asserts.checkerror('contourc()', getString(message('Nelson:notEnoughInputs')));
asserts.checkerror('contourc([1 2; 3 4], [2 1])', getString(message('Nelson:normalizeContourLevels:contourLevelsNotIncreasing')));
asserts.checkerror('contourc([1 2; 3 4], [NaN 1])', getString(message('Nelson:normalizeContourLevels:contourLevelsNotFinite')));
asserts.checkerror('contourc([1 1], [1 2], [1 2; 3 4])', getString(message('Nelson:validateContourData:mustBeMonotonic', 'X')));
asserts.checkerror('contourc([1 2 3], [1 2], [1 2; 3 4], [2 2])', getString(message('Nelson:validateContourData:vectorLengthMismatch', 'X')));
%=============================================================================
