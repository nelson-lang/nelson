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
f = figure();
ax1 = axes('Position', [0.1 0.1 0.7 0.7]);
ax2 = axes('Position', [0.65 0.65 0.28 0.28]);
x = linspace(0,10);
y1 = sin(x);
y2 = cos(x);
plot(ax1, x, y1);
assert_isequal(ax2, gca());
plot(ax2, x, y2);
assert_isequal(ax2, gca());
%=============================================================================
axes(ax1)
assert_isequal(ax1, gca());
%=============================================================================
R = properties(gca());
REF = {'ALim';
'ALimMode';
'AlphaMap';
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
'DataLimits';
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
'PositionMode';
'Projection';
'Selected';
'SelectionHighlight';
'Tag';
'TextHeight';
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
assert_isequal(R, REF);
%=============================================================================
f = figure();
ax1 = axes('Position',[0.1 0.1 .6 .6],'Box','on');
ax2 = axes('Position',[.35 .35 .6 .6],'Box','on');
assert_isequal(f.CurrentAxes, ax2);
axes(ax1);
x = linspace(0,10);
y = sin(x);
plot(x,y)
assert_isequal(f.CurrentAxes, ax1);
%=============================================================================
f1 = figure();
f2 = figure();
ax1 = axes(f1, 'Position', [0.1 0.1 0.7 0.7]);
ax2 = axes(f2, 'Position', [0.65 0.65 0.28 0.28]);
ax3 = axes(f1, 'Position', [0.2 0.2 0.5 0.5]);
ax4 = axes(f2, 'Position', [0.3 0.3 0.4 0.4]);
assert_isequal(ax4.Parent, f2);
%=============================================================================
axes(ax1);
assert_isequal(ax1.Parent, f1);
%=============================================================================
f1 = figure();
f2 = figure();
ax = axes(f1);
assert_isequal(ax.Parent, f1);
cf = gcf();
assert_isequal(cf, f2);
%=============================================================================
