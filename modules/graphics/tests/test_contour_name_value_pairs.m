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
% EdgeColor/LineColor and FaceColor/Fill are two names each for one attribute.
% A name/value pair given by the caller must reach the object whichever of the
% two names it uses, and the last name written wins.
%=============================================================================
f = figure('Visible', 'off');
Z = peaks(5);
%=============================================================================
% Defaults, unchanged by this: unfilled contours draw from the colormap, filled
% ones draw a dark gray edge.
[M, h] = contour(Z);
asserts.isequal(h.LineColor, 'flat');
asserts.isequal(h.EdgeColor, 'flat');
asserts.isequal(h.FaceColor, 'none');
asserts.isequal(char(h.Fill), 'off');
clf(f);
%=============================================================================
[M, h] = contourf(Z);
asserts.isapprox(h.LineColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isapprox(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isequal(h.FaceColor, 'flat');
asserts.isequal(char(h.Fill), 'on');
clf(f);
%=============================================================================
% LineColor given at construction reaches the object, and drives EdgeColor.
[M, h] = contour(Z, 'LineColor', 'r');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
clf(f);
%=============================================================================
[M, h] = contourf(Z, 'LineColor', 'r');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
clf(f);
%=============================================================================
[M, h] = contour3(Z, 'LineColor', 'b');
asserts.isequal(h.LineColor, [0 0 1]);
asserts.isequal(h.EdgeColor, [0 0 1]);
clf(f);
%=============================================================================
% The other name of the pair works too.
[M, h] = contour(Z, 'EdgeColor', 'r');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
clf(f);
%=============================================================================
[M, h] = contourf(Z, 'EdgeColor', 'r');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
clf(f);
%=============================================================================
% Both names in the same call: the last one written wins.
[M, h] = contour(Z, 'EdgeColor', 'r', 'LineColor', 'b');
asserts.isequal(h.LineColor, [0 0 1]);
asserts.isequal(h.EdgeColor, [0 0 1]);
clf(f);
%=============================================================================
[M, h] = contour(Z, 'LineColor', 'b', 'EdgeColor', 'r');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
clf(f);
%=============================================================================
% Colorless and colormap-driven spellings survive the pair as well.
[M, h] = contour(Z, 'LineColor', 'none');
asserts.isequal(h.LineColor, 'none');
asserts.isequal(h.EdgeColor, 'none');
clf(f);
%=============================================================================
[M, h] = contourf(Z, 'LineColor', 'flat');
asserts.isequal(h.LineColor, 'flat');
asserts.isequal(h.EdgeColor, 'flat');
asserts.isequal(char(h.Fill), 'on');
clf(f);
%=============================================================================
% Fill given at construction reaches the object, and drives FaceColor.
[M, h] = contour(Z, 'Fill', 'on');
asserts.isequal(char(h.Fill), 'on');
asserts.isequal(h.FaceColor, 'flat');
clf(f);
%=============================================================================
[M, h] = contourf(Z, 'Fill', 'off');
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
clf(f);
%=============================================================================
[M, h] = contour(Z, 'FaceColor', 'flat');
asserts.isequal(char(h.Fill), 'on');
asserts.isequal(h.FaceColor, 'flat');
clf(f);
%=============================================================================
[M, h] = contourf(Z, 'FaceColor', 'none');
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
clf(f);
%=============================================================================
% Both names of the fill pair in the same call: the last one written wins.
[M, h] = contour(Z, 'FaceColor', 'flat', 'Fill', 'off');
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
clf(f);
%=============================================================================
[M, h] = contour(Z, 'Fill', 'on', 'FaceColor', 'none');
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
clf(f);
%=============================================================================
% A pair does not swallow the ones written next to it.
[M, h] = contour(Z, 'LineWidth', 3, 'LineColor', 'r', 'LineStyle', '--');
asserts.isequal(h.LineWidth, 3);
asserts.isequal(h.LineStyle, '--');
asserts.isequal(h.LineColor, [1 0 0]);
clf(f);
%=============================================================================
% Setting either name after construction keeps both in step.
[M, h] = contour(Z);
h.LineColor = 'r';
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
h.EdgeColor = 'g';
asserts.isequal(h.LineColor, [0 1 0]);
asserts.isequal(h.EdgeColor, [0 1 0]);
h.Fill = 'on';
asserts.isequal(char(h.Fill), 'on');
asserts.isequal(h.FaceColor, 'flat');
h.FaceColor = 'none';
asserts.isequal(char(h.Fill), 'off');
asserts.isequal(h.FaceColor, 'none');
clf(f);
%=============================================================================
% fcontour is a contour that brings its own LineColor and Fill, replacing the
% ones the contour registered. The pair has to keep working there too.
h = fcontour(@(x, y) x + y, [0 1 0 1], 'LineColor', 'r');
asserts.isequal(h.LineColor, [1 0 0]);
asserts.isequal(h.EdgeColor, [1 0 0]);
%=============================================================================
close(f);
%=============================================================================
