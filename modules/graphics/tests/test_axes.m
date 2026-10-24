%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
ax1 = axes('Position', [0.1 0.1 0.7 0.7]);
ax2 = axes('Position', [0.65 0.65 0.28 0.28]);
x = linspace(0, 10);
y1 = sin(x);
y2 = cos(x);
plot(ax1, x, y1);
asserts.isequal(ax2, gca());
plot(ax2, x, y2);
asserts.isequal(ax2, gca());
%=============================================================================
axes(ax1)
asserts.isequal(ax1, gca());
%=============================================================================
R = properties(gca());
REF = {'ALim';
  'ALimMode';
  'Alphamap';
  'AmbientLightColor';
  'BeingDeleted';
  'Box';
  'BusyAction';
  'CLim';
  'CLimMode';
  'CameraPosition';
  'CameraPositionMode';
  'CameraTarget';
  'CameraTargetMode';
  'CameraUpVector';
  'CameraUpVectorMode';
  'CameraViewAngle';
  'CameraViewAngleMode';
  'Children';
  'Clipping';
  'Color';
  'ColorOrder';
  'ColorOrderIndex';
  'Colormap';
  'CreateFcn';
  'DataAspectRatio';
  'DataAspectRatioMode';
  'DeleteFcn';
  'FontAngle';
  'FontName';
  'FontSize';
  'FontUnits';
  'FontWeight';
  'GridAlpha';
  'GridColor';
  'GridLineStyle';
  'HandleVisibility';
  'HitTest';
  'Interruptible';
  'Layer';
  'Layout';
  'LineStyleOrder';
  'LineStyleOrderIndex';
  'LineWidth';
  'MinorGridLineStyle';
  'NextPlot';
  'OuterPosition';
  'Parent';
  'PlotBoxAspectRatio';
  'PlotBoxAspectRatioMode';
  'Position';
  'Projection';
  'Selected';
  'SelectionHighlight';
  'Tag';
  'TickDir';
  'TickDirMode';
  'TickLabelInterpreter';
  'TickLength';
  'TightInset';
  'Title';
  'Type';
  'Units';
  'UserData';
  'View';
  'Visible';
  'XAxisLocation';
  'XColor';
  'XDir';
  'XGrid';
  'XLabel';
  'XLim';
  'XLimMode';
  'XMinorGrid';
  'XScale';
  'XTick';
  'XTickLabel';
  'XTickLabelMode';
  'XTickMode';
  'YAxisLocation';
  'YColor';
  'YDir';
  'YGrid';
  'YLabel';
  'YLim';
  'YLimMode';
  'YMinorGrid';
  'YScale';
  'YTick';
  'YTickLabel';
  'YTickLabelMode';
  'YTickMode';
  'ZColor';
  'ZDir';
  'ZGrid';
  'ZLabel';
  'ZLim';
  'ZLimMode';
  'ZMinorGrid';
  'ZScale';
  'ZTick';
  'ZTickLabel';
  'ZTickLabelMode';
  'ZTickMode'};
asserts.istrue(all(ismember(REF, R)));
asserts.isequal(gca().LineWidth, 0.5);
asserts.isequal(gca().TickDir, 'in');
asserts.isequal(gca().TickDirMode, 'auto');
%=============================================================================
f = figure();
ax1 = axes('Position', [0.1 0.1 .6 .6], 'Box', 'on');
ax2 = axes('Position', [.35 .35 .6 .6], 'Box', 'on');
asserts.isequal(f.CurrentAxes, ax2);
axes(ax1);
x = linspace(0, 10);
y = sin(x);
plot(x, y)
asserts.isequal(f.CurrentAxes, ax1);
%=============================================================================
f1 = figure();
f2 = figure();
ax1 = axes(f1, 'Position', [0.1 0.1 0.7 0.7]);
ax2 = axes(f2, 'Position', [0.65 0.65 0.28 0.28]);
ax3 = axes(f1, 'Position', [0.2 0.2 0.5 0.5]);
ax4 = axes(f2, 'Position', [0.3 0.3 0.4 0.4]);
asserts.isequal(ax4.Parent, f2);
%=============================================================================
axes(ax1);
asserts.isequal(ax1.Parent, f1);
%=============================================================================
f1 = figure();
f2 = figure();
ax = axes(f1);
asserts.isequal(ax.Parent, f1);
cf = gcf();
asserts.isequal(cf, f2);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
ax.XLim = [0 1];
ax.YLim = [0 1];
drawnow();
asserts.isequal(char(ax.Box), 'off');
asserts.isapprox(ax.XTick, 0:0.1:1, 1e-12);
asserts.isapprox(ax.YTick, 0:0.1:1, 1e-12);
ax.XLim = [0.5 2.5];
ax.YLim = [-4 10];
drawnow();
asserts.isapprox(ax.XTick, 0.5:0.5:2.5, 1e-12);
asserts.isapprox(ax.YTick, -4:2:10, 1e-12);
close(f);
%=============================================================================
