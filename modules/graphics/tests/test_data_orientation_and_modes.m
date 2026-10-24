%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% area: a row vector input keeps its row orientation and X stays auto.
f = figure();
h = area(1:5);
asserts.isequal(h.XData, [1 2 3 4 5]);
asserts.isequal(h.YData, [1 2 3 4 5]);
asserts.isequal(h.XDataMode, 'auto');
close(f);
%=============================================================================
% area: a column vector input keeps its column orientation.
f = figure();
h = area((1:5)');
asserts.isequal(h.XData, (1:5)');
asserts.isequal(h.YData, (1:5)');
asserts.isequal(h.XDataMode, 'auto');
close(f);
%=============================================================================
% area(X, Y): an explicit X switches XDataMode to manual, orientation kept.
f = figure();
h = area(1:5, 2:6);
asserts.isequal(h.XData, [1 2 3 4 5]);
asserts.isequal(h.YData, [2 3 4 5 6]);
asserts.isequal(h.XDataMode, 'manual');
close(f);
%=============================================================================
% boxchart: a column vector of values is stored (and read back) as a row.
f = figure();
h = boxchart((1:10)');
asserts.isequal(h.YData, [1 2 3 4 5 6 7 8 9 10]);
asserts.isequal(h.YDataMode, 'manual');
close(f);
%=============================================================================
% stackedplot: the chart XData sample index is a row vector.
f = figure();
s = stackedplot([1 2; 3 4; 5 6]);
asserts.isequal(s.XData, [1 2 3]);
close(f);
%=============================================================================
% scatter: an explicit Y is manual, the auto color-order color keeps C auto.
f = figure();
s = scatter(1:5, 1:5);
asserts.isequal(s.XDataMode, 'manual');
asserts.isequal(s.YDataMode, 'manual');
asserts.isequal(s.ZDataMode, 'auto');
asserts.isequal(s.CDataMode, 'auto');
close(f);
%=============================================================================
% scatter with an explicit color: CDataMode becomes manual.
f = figure();
s = scatter(1:5, 1:5, 36, [1 0 0]);
asserts.isequal(s.CDataMode, 'manual');
close(f);
%=============================================================================
% scatter3: explicit X, Y and Z are all manual, C stays auto.
f = figure();
s = scatter3(1:5, 1:5, 1:5);
asserts.isequal(s.XDataMode, 'manual');
asserts.isequal(s.YDataMode, 'manual');
asserts.isequal(s.ZDataMode, 'manual');
asserts.isequal(s.CDataMode, 'auto');
close(f);
%=============================================================================
% bubblechart: explicit Y is manual, auto color keeps C auto.
f = figure();
b = bubblechart(1:5, 1:5, 1:5);
asserts.isequal(b.XDataMode, 'manual');
asserts.isequal(b.YDataMode, 'manual');
asserts.isequal(b.CDataMode, 'auto');
close(f);
%=============================================================================
% swarmchart: routes through scatter, same auto/manual semantics.
f = figure();
s = swarmchart(ones(1, 10), 1:10);
asserts.isequal(s.YDataMode, 'manual');
asserts.isequal(s.CDataMode, 'auto');
close(f);
%=============================================================================
