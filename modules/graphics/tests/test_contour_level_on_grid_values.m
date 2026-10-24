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
% A level landing exactly on values of the grid walks through grid vertices.
% The cell that holds such a vertex is cut twice at it and crossed without
% moving, and four pieces of line can meet at one pair of coordinates: a line
% put back together by comparing coordinates comes out in pieces.
%=============================================================================
% every level of this surface runs on grid vertices from end to end
Z = repmat((1:8)', 1, 8) + repmat(1:8, 8, 1);
M = contourc(Z, 4:2:16);
levels = 4:2:16;
lines = {[3 2 1; 1 2 3], ...
    [5 4 3 2 1; 1 2 3 4 5], ...
    [7 6 5 4 3 2 1; 1 2 3 4 5 6 7], ...
    [2 3 4 5 6 7 8; 8 7 6 5 4 3 2], ...
    [4 5 6 7 8; 8 7 6 5 4], ...
    [6 7 8; 8 7 6], ...
    [8; 8]};
expected = [];
for k = 1:numel(lines)
  expected = [expected, [levels(k); size(lines{k}, 2)], lines{k}];
end
asserts.isequal(M, expected);
%=============================================================================
% the same diagonal on a wider grid, as one line and not eleven
M = contourc(repmat((1:11)', 1, 11) + repmat(1:11, 11, 1), [12 12]);
asserts.isequal(M, [12, 11:-1:1; 11, 1:11]);
%=============================================================================
% a level the surface never goes below is drawn along the plateau, once
Z = repmat((1:8)', 1, 8);
asserts.isequal(contourc(Z, [1 1]), [1, 1:8; 8, ones(1, 8)]);
asserts.isequal(contourc(Z, [8 8]), [8, 1:8; 8, 8 * ones(1, 8)]);
asserts.isequal(contourc(Z, [4 4]), [4, 1:8; 8, 4 * ones(1, 8)]);
%=============================================================================
% a level touching the surface at a single grid point is a line of one vertex
Z = repmat((1:8)', 1, 8) + repmat(1:8, 8, 1);
asserts.isequal(contourc(Z, [16 16]), [16 8; 1 8]);
asserts.isequal(contourc(Z, [2 2]), [2 1; 1 1]);
%=============================================================================
% no line may stand still: a vertex repeated twice in a row is a piece of
% another line left inside this one
grids = {repmat((1:8)', 1, 8) + repmat(1:8, 8, 1), ...
    (repmat((1:9)', 1, 9) - 5) .^ 2 + (repmat(1:9, 9, 1) - 5) .^ 2, ...
    repmat((1:9)', 1, 9) .* repmat(1:9, 9, 1), ...
    abs(repmat((1:9)', 1, 9) - repmat(1:9, 9, 1)), ...
    min(abs(repmat((1:9)', 1, 9) - 5), abs(repmat(1:9, 9, 1) - 5))};
for k = 1:numel(grids)
  Z = grids{k};
  M = contourc(Z, unique(Z(:))');
  col = 1;
  while col <= size(M, 2)
    n = M(2, col);
    pts = M(:, col + 1:col + n);
    asserts.istrue(n >= 1);
    asserts.istrue(all(any(diff(pts, 1, 2) ~= 0, 1)));
    col = col + n + 1;
  end
  asserts.isequal(col, size(M, 2) + 1);
end
%=============================================================================
% a line that stops where a cell put out of use by a NaN left the same
% crossing behind is one line, not two
Z = (repmat((1:7)', 1, 7) - 4) .^ 2 + (repmat(1:7, 7, 1) - 4) .^ 2;
Z(1, :) = NaN;
M = contourc(Z, [9 9]);
asserts.isequal(size(M, 2), 16);
asserts.isequal(M(:, 1), [9; 15]);
%=============================================================================
% a level crossing itself on the grid: four arms, each walked once
Z = min(abs(repmat((1:9)', 1, 9) - 5), abs(repmat(1:9, 9, 1) - 5));
M = contourc(Z, [0 0]);
counts = [];
col = 1;
while col <= size(M, 2)
  counts = [counts, M(2, col)];
  col = col + M(2, col) + 1;
end
asserts.isequal(counts, [8 8 8 8]);
%=============================================================================
