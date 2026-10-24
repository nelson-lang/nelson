%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
figure();
t = hgtransform();
ax = gca();
asserts.istrue(isempty(t.Children));
asserts.isequal(t.Parent, ax);
asserts.isequal(t.Type, 'hgtransform');
asserts.istrue(isgraphics(t, 'hgtransform'));
asserts.istrue(t.Visible == 'on');
asserts.isequal(t.Matrix, eye(4));
%=============================================================================
% Matrix is settable and read back.
figure();
t = hgtransform();
M = makehgtform('translate', [3 2 1]);
t.Matrix = M;
asserts.isequal(t.Matrix, M);
set(t, 'Matrix', makehgtform('zrotate', pi / 2));
asserts.isapprox(t.Matrix, makehgtform('zrotate', pi / 2), 1e-12);
%=============================================================================
% A transform can parent another transform (nesting) and ordinary objects.
figure();
ax = gca();
tOuter = hgtransform('Parent', ax);
tInner = hgtransform('Parent', tOuter);
asserts.isequal(tInner.Parent, tOuter);
asserts.isequal(ancestor(tInner, 'axes'), ax);
L = line([0 1], [0 0], 'Parent', tInner, 'Color', 'r');
asserts.isequal(L.Parent, tInner);
%=============================================================================
% Reparenting an existing object under a transform moves it between children.
figure();
ax = gca();
t = hgtransform('Parent', ax);
L = line([0 1], [0 1], 'Color', 'b');
set(L, 'Parent', t);
asserts.isequal(L.Parent, t);
asserts.istrue(any(t.Children == L));
%=============================================================================
% Matrix must be an exposed property.
figure();
t = hgtransform();
asserts.istrue(any(strcmp(properties(t), 'Matrix')));
%=============================================================================
% Rendering: the Matrix must actually transform the children at render time, not
% merely be stored. A vertical red line at x = 0 is translated by +5 data units
% in x; its rendered pixel column centroid must move to the right. Without the
% renderer composing the transform this stays put and the test fails.
f = figure('Visible', 'off');
ax = axes('Parent', f);
axis(ax, [-10 10 -10 10]);
axis(ax, 'manual');
t = hgtransform('Parent', ax);
line([0 0], [-8 8], 'Parent', t, 'Color', 'r', 'LineWidth', 3);
cx0 = localRedCentroidX(getframe(f));
set(t, 'Matrix', makehgtform('translate', [5 0 0]));
cx1 = localRedCentroidX(getframe(f));
asserts.istrue(~isnan(cx0));
asserts.istrue(~isnan(cx1));
% +5 of a 20-unit span is a quarter of the axes width; require a clear rightward
% move (half of that, to stay robust to the axes inset and anti-aliasing).
asserts.istrue((cx1 - cx0) > 0.125 * size(getframe(f).cdata, 2));
close(f);
%=============================================================================
% Rendering: a zrotate must reorient the children. A horizontal line becomes
% vertical, so its pixel bounding box flips from wide-and-short to tall-and-thin.
f = figure('Visible', 'off');
ax = axes('Parent', f);
axis(ax, [-10 10 -10 10]);
axis(ax, 'manual');
t = hgtransform('Parent', ax);
line([0 8], [0 0], 'Parent', t, 'Color', 'r', 'LineWidth', 3);
[rows0, cols0] = localRedSpan(getframe(f));
set(t, 'Matrix', makehgtform('zrotate', pi / 2));
[rows1, cols1] = localRedSpan(getframe(f));
asserts.istrue(cols0 > rows0);
asserts.istrue(rows1 > cols1);
close(f);
%=============================================================================
function cx = localRedCentroidX(frame)
  c = frame.cdata;
  mask = (c(:, :, 1) > 150) & (c(:, :, 2) < 100) & (c(:, :, 3) < 100);
  [~, cols] = find(mask);
  if isempty(cols)
    cx = NaN;
  else
    cx = mean(cols);
  end
end
%=============================================================================
function [rowSpan, colSpan] = localRedSpan(frame)
  c = frame.cdata;
  mask = (c(:, :, 1) > 150) & (c(:, :, 2) < 100) & (c(:, :, 3) < 100);
  [rows, cols] = find(mask);
  rowSpan = max(rows) - min(rows);
  colSpan = max(cols) - min(cols);
end
%=============================================================================
