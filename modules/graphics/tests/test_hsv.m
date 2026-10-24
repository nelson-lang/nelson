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
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
f = figure();
surf(peaks);
colormap('hsv');
r = f.Colormap;
asserts.isequal(size(r, 2), 3);
asserts.isequal(r(1, :), [1, 0, 0]);
ax = gca();
asserts.isequal(ax.XLim, [0 50]);
asserts.isequal(ax.YLim, [0 50]);
asserts.isequal(ax.ZLim, [-10 10]);
asserts.isequal(ax.XTick, 0:10:50);
asserts.isequal(ax.YTick, 0:10:50);
asserts.isequal(ax.ZTick, -10:5:10);
asserts.isapprox(ax.PlotBoxAspectRatio, [1 0.894887910173428 0.718076889578469], 1e-12);
r = hsv(64);
ref = [
  1.0000, 0, 0;
  1.0000, 0.0938, 0;
  1.0000, 0.1875, 0;
  1.0000, 0.2812, 0];
asserts.isapprox(r(1:4, :), ref, 1e-4);
asserts.isapprox(hsv(8), [
  1.0000, 0, 0;
  1.0000, 0.7500, 0;
  0.5000, 1.0000, 0;
  0, 1.0000, 0.2500;
  0, 1.0000, 1.0000;
  0, 0.2500, 1.0000;
  0.5000, 0, 1.0000;
  1.0000, 0, 0.7500], 1e-4);
%=============================================================================
