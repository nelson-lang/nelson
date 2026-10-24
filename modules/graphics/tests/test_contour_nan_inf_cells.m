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
% Only a NaN corner takes a cell out of use. An infinite corner still says
% which side of the level it is on, and the cell is cut like any other. A cell
% a NaN has put out of use keeps the crossings on the edges whose two ends can
% still be compared. Every expected value below was measured, none was derived.
%=============================================================================
% One infinite corner, the other three finite: the cell is cut, and the
% crossing on the edge that runs into the infinity sits on the finite end of
% that edge, which is where it goes as the other end runs away.
asserts.isequal(contourc([Inf 1; -1 1], [0 0]), [0 1.5 1; 2 2 2]);
asserts.isequal(contourc([-Inf 1; -1 1], [0 0]), [0 1.5 2; 2 2 1]);
%=============================================================================
% The same, once for each way the edge can run: the crossing lands on the
% finite end whether the infinity is the left or the right end of a row, the
% bottom or the top end of a column.
asserts.isequal(contourc([Inf -1; 1 -1], [0 0]), [0 1.5 2; 2 2 1]);
asserts.isequal(contourc([-1 Inf; -1 1], [0 0]), [0 1.5 1; 2 2 1]);
asserts.isequal(contourc([Inf 1; -1 -1], [0 0]), [0 1 2; 2 2 1.5]);
asserts.isequal(contourc([-1 -1; Inf 1], [0 0]), [0 1 2; 2 1 1.5]);
%=============================================================================
% Both ends of the cut edge infinite: the crossing has nowhere to converge to
% and stays on the end the grid reaches first, the left end of a row and the
% top end of a column. Which end holds the plus and which the minus makes no
% difference: the bottom edge is cut at x = 1 either way, the left edge at
% y = 2 either way.
asserts.isequal(contourc([-Inf Inf; -1 1], [0 0]), [0 1.5 1; 2 2 1]);
asserts.isequal(contourc([Inf -Inf; 1 -1], [0 0]), [0 1.5 1; 2 2 1]);
asserts.isequal(contourc([-Inf -1; Inf 1], [0 0]), [0 1 2; 2 2 1.5]);
asserts.isequal(contourc([Inf 1; -Inf -1], [0 0]), [0 1 2; 2 2 1.5]);
%=============================================================================
% An infinity in the middle of a surface does not blow a hole in the level
% line the way a NaN does: the same block of points read as Inf leaves the
% line whole, read as NaN cuts it into four.
Z = peaks(12);
Zinf = Z; Zinf(5:7, 5:7) = Inf;
Znan = Z; Znan(5:7, 5:7) = NaN;
Minf = contourc(Zinf, [0 0]);
Mnan = contourc(Znan, [0 0]);
asserts.isequal(size(Minf), [2 35]);
asserts.isequal(Minf(2, 1), 16);
asserts.isequal(Minf(2, 18), 17);
asserts.isequal(size(Mnan), [2 32]);
%=============================================================================
% The line runs along the rim of the block of infinities instead of stopping
% at it: the crossings on the edges reaching into the block sit on the finite
% ends, which are grid points.
asserts.isequal(Minf(:, 9:10), [6 7; 4 4]);
asserts.isequal(Minf(:, 27:29), [5 4 4; 8 7 6]);
%=============================================================================
% A cell a NaN has put out of use still carries the crossing on an edge whose
% two ends are finite. A surface rising with x, cut halfway across a column of
% cells, gives one straight line.
R = repmat((1:9) - 5.5, 9, 1);
asserts.isequal(contourc(R, [0 0]), [0 5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5 5.5; ...
  9 9 8 7 6 5 4 3 2 1]);
%=============================================================================
% One NaN breaks the line in two, and the crossing on the edge it leaves
% usable is taken into the piece that reaches it.
R1 = R;
R1(4, 5) = NaN;
asserts.isequal(contourc(R1, [0 0]), [0 5.5 5.5 5.5 5.5 5.5 0 5.5 5.5 5.5; ...
  5 5 6 7 8 9 3 3 2 1]);
%=============================================================================
% A second NaN leaves the edge between them bordered by two unusable cells.
% The crossing on it joins nothing and comes out as a line of a single vertex,
% written once for the two cells that share the edge.
R2 = R1;
R2(6, 5) = NaN;
asserts.isequal(contourc(R2, [0 0]), [0 5.5 5.5 5.5 0 5.5 0 5.5 5.5 5.5; ...
  3 7 8 9 1 5 3 3 2 1]);
%=============================================================================
% A grid sown with NaN leaves most of its cells out of use: what survives is
% fifteen lines, twelve of them a lone crossing.
C = peaks(12);
C(3:2:11, 3:2:11) = NaN;
M = contourc(C, [0 0]);
asserts.isequal(size(M), [2 35]);
col = 1;
lineCount = 0;
loneCount = 0;
while col <= size(M, 2)
  n = M(2, col);
  lineCount = lineCount + 1;
  if n == 1
    loneCount = loneCount + 1;
  end
  col = col + n + 1;
end
asserts.isequal(lineCount, 15);
asserts.isequal(loneCount, 12);
%=============================================================================
% A NaN on its own, on the edge of the grid, and in a corner of it, each leave
% the rest of the surface alone.
N1 = Z; N1(6, 6) = NaN;
asserts.isequal(size(contourc(N1, [0 0])), [2 44]);
N2 = Z; N2(1, 6) = NaN;
asserts.isequal(size(contourc(N2, [0 0])), [2 43]);
N3 = Z; N3(1, 1) = NaN;
asserts.isequal(size(contourc(N3, [0 0])), [2 43]);
%=============================================================================
% A cell with no edge left to compare draws nothing at all.
asserts.isequal(size(contourc([NaN 1; NaN 1], [0 0])), [2 0]);
asserts.isequal(size(contourc([NaN 1; -1 NaN], [0 0])), [2 0]);
asserts.isequal(size(contourc([NaN NaN; NaN NaN], [0 0])), [2 0]);
%=============================================================================
% A surface that never reaches the level has no line, infinities and all.
asserts.isequal(size(contourc([Inf 1; -1 1], [-5 -5])), [2 0]);
%=============================================================================
