%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% A contour line crossing N cells is N + 1 vertices long, not 2N: the vertex
% two neighbouring cells meet on belongs to both and is written once. The
% vertices come out in the order that keeps the part of the surface above the
% level on the left of the walk. Every expected value below was measured, none
% was derived.
%=============================================================================
% The whole reported case: a surface rising with x, cut at one level, gives a
% straight line down the middle of the grid. Four cells across, five vertices.
f = figure('Visible', 'off');
Z = repmat(linspace(0, 10, 5), 5, 1);
[M, h] = contour(Z, [4 4]);
asserts.isequal(size(M), [2 6]);
asserts.isequal(M(:, 1), [4; 5]);
asserts.isequal(M(:, 2:end), [2.6 2.6 2.6 2.6 2.6; 5 4 3 2 1]);
close(f);
%=============================================================================
% The level line runs down, not up: the surface rises with x, so walking from
% y = 5 to y = 1 keeps the high side on the left.
M = contourc(repmat(linspace(0, 10, 5), 5, 1), [4 4]);
asserts.istrue(M(2, 2) > M(2, size(M, 2)));
%=============================================================================
% Coordinates given by hand are carried through unchanged.
M = contourc(10:10:50, 100:100:500, repmat(linspace(0, 10, 5), 5, 1), [4 4]);
asserts.isequal(size(M), [2 6]);
asserts.isequal(M(:, 2:end), [26 26 26 26 26; 500 400 300 200 100]);
%=============================================================================
% At scale: the whole matrix, headers included.
asserts.isequal(size(contourc(peaks(5)), 2), 55);
asserts.isequal(size(contourc(peaks(20)), 2), 211);
asserts.isequal(size(contourc(peaks(40)), 2), 424);
%=============================================================================
% Each header announces the number of vertices that follow it, and the columns
% add up exactly.
M = contourc(peaks(20));
col = 1;
while col <= size(M, 2)
  n = M(2, col);
  asserts.istrue(n >= 2);
  asserts.istrue(col + n <= size(M, 2));
  col = col + n + 1;
end
asserts.isequal(col, size(M, 2) + 1);
%=============================================================================
% No vertex is written twice in a row anywhere in that matrix.
col = 1;
while col <= size(M, 2)
  n = M(2, col);
  pts = M(:, col + 1:col + n);
  asserts.istrue(isempty(find(all(diff(pts, 1, 2) == 0, 1))));
  col = col + n + 1;
end
%=============================================================================
% A closed line does come back to the vertex it started from, and keeps that
% one: two square blobs give two diamonds of four corners, written as five
% vertices each.
M = contourc([0 0 0 0 0; 0 5 0 5 0; 0 0 0 0 0], [2 2]);
asserts.isequal(size(M), [2 12]);
asserts.isequal(M(2, 1), 5);
asserts.isequal(M(2, 7), 5);
left = M(:, 2:6);
right = M(:, 8:12);
asserts.isequal(left(:, 1), left(:, 5));
asserts.isequal(right(:, 1), right(:, 5));
asserts.isequal(sort(left(1, 1:4)), [1.4 2 2 2.6]);
asserts.isequal(sort(left(2, 1:4)), [1.4 2 2 2.6]);
asserts.isequal(sort(right(1, 1:4)), [3.4 4 4 4.6]);
asserts.isequal(sort(right(2, 1:4)), [1.4 2 2 2.6]);
%=============================================================================
% Round a peak the walk turns one way, round a pit the other: the side above
% the level is on the left either way. The signed area says which.
M = contourc(peaks(20), [4 4]);
pts = M(:, 2:end);
asserts.isequal(size(pts, 2), 21);
asserts.isequal(pts(:, 1), pts(:, end));
peakArea = sum(pts(1, 1:end - 1) .* pts(2, 2:end) - pts(1, 2:end) .* pts(2, 1:end - 1)) / 2;
asserts.istrue(peakArea > 0);
%=============================================================================
M = contourc(peaks(20), [-4 -4]);
pts = M(:, 2:end);
asserts.isequal(size(pts, 2), 15);
asserts.isequal(pts(:, 1), pts(:, end));
pitArea = sum(pts(1, 1:end - 1) .* pts(2, 2:end) - pts(1, 2:end) .* pts(2, 1:end - 1)) / 2;
asserts.istrue(pitArea < 0);
%=============================================================================
% A saddle cell is cut on all four of its edges, and the two arcs can pair
% those crossings either way: round the bottom-left and top-right corners, or
% round the other two. The level line goes round the bottom-left and
% top-right pair exactly when the mean of the four corners lands between the
% level and the top-left corner, the level end of that band excluded.
%
% What each case below asserts is which crossings the two arcs join, so the
% cells are read back with the endpoints of an arc, and then the two arcs,
% put in a fixed order. The order the lines come out in and the way round
% each one is walked are not what these cells are here to pin down.
%
% Cells 1 and 2 have the mean exactly on the level, once inside the band and
% once outside it, the top-left corner having changed sides. Cells 3 to 5
% have a mean past the level, kept there by two corners that a third, much
% further from the level, would outweigh under any other reading. Cells 6 and
% 7 are past the far end of the band, where one corner far enough beyond the
% level drags the mean beyond the top-left corner as well.
saddleCells = {[2 0; 0 2], [0 2; 2 0], [1 -7; -7 25], [-7 1; 25 -7], ...
  [25 -7; -7 1], [1 -7; -1 1], [-7 1; 1 -1]};
saddleLevels = [1 1 0 0 0 0 0];
saddleArcs = {[1 1.5 1.5 1; 1.5 2 2 1.5], [1 1.5 1.5 2; 1.5 1 2 1.5], ...
  [1 1.125 1.21875 2; 1.125 1 2 1.21875], [1 1.21875 1.875 1; 1.78125 2 2 1.125], ...
  [1 1.78125 1.875 2; 1.78125 1 2 1.875], [1 1.5 1.5 2; 1.125 1 2 1.875], ...
  [1 1.875 1.5 2; 1.875 1 2 1.5]};
for k = 1:numel(saddleCells)
  M = contourc(saddleCells{k}, [saddleLevels(k) saddleLevels(k)]);
  asserts.isequal(size(M), [2 6]);
  asserts.isequal(M(2, [1 4]), [2 2]);
  first = sortrows([M(:, 2)'; M(:, 3)']);
  second = sortrows([M(:, 5)'; M(:, 6)']);
  asserts.isequal(sortrows([first(1, :), first(2, :); second(1, :), second(2, :)]), saddleArcs{k});
end
%=============================================================================
