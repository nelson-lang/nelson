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
close all
%=============================================================================
assert_properties(groot(), {'FixedWidthFontName'; 'HandleVisibility'; 'MonitorPositions'; 'ShowHiddenHandles'});
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
asserts.isequal(f.Position, [100 100 560 420]);
assert_reference_properties(f, reference_figure_properties(), ...
  {'BeingDeleted'; 'Number'; 'Parent'; 'Type'});
assert_properties(f, {'AutoResizeChildren'; 'Clipping'; 'ContextMenu'; 'CurrentCharacter';
  'CurrentPoint'; 'DockControls'; 'FileName'; 'Icon'; 'InnerPosition'; 'IntegerHandle';
  'InvertHardcopy'; 'OuterPosition'; 'PaperOrientation'; 'PaperPosition';
  'PaperPositionMode'; 'PaperSize'; 'PaperType'; 'PaperUnits'; 'Pointer';
  'PointerShapeCData'; 'PointerShapeHotSpot'; 'Renderer'; 'RendererMode'; 'Scrollable';
  'SelectionType'; 'Theme'; 'ThemeChangedFcn'; 'ThemeMode'; 'Units';
  'WindowButtonDownFcn'; 'WindowButtonMotionFcn'; 'WindowButtonUpFcn';
  'WindowKeyPressFcn'; 'WindowKeyReleaseFcn'; 'WindowScrollWheelFcn'; 'WindowStyle'});
%=============================================================================
ax = axes('Parent', f);
assert_reference_properties(ax, reference_axes_properties(), {'BeingDeleted'; 'Type'});
assert_properties(ax, {'AlphaScale'; 'BoxStyle'; 'ButtonDownFcn'; 'ClippingStyle';
  'ColorScale'; 'ContextMenu'; 'CurrentPoint'; 'FontSizeMode'; 'FontSmoothing';
  'GridAlphaMode'; 'GridColorMode'; 'GridLineWidth'; 'GridLineWidthMode';
  'InnerPosition'; 'InteractionOptions'; 'Interactions'; 'LabelFontSizeMultiplier';
  'Legend'; 'LineStyleCyclingMethod'; 'MinorGridAlpha'; 'MinorGridAlphaMode';
  'MinorGridColor'; 'MinorGridColorMode'; 'MinorGridLineWidth'; 'MinorGridLineWidthMode';
  'NextSeriesIndex'; 'PickableParts'; 'PositionConstraint'; 'SortMethod'; 'Subtitle';
  'SubtitleFontWeight'; 'TitleFontSizeMultiplier'; 'TitleFontWeight';
  'TitleHorizontalAlignment'; 'Toolbar'; 'XAxis'; 'XColorMode'; 'XLimitMethod';
  'XMinorTick'; 'XTickLabelRotation'; 'XTickLabelRotationMode'; 'YAxis'; 'YColorMode';
  'YLimitMethod'; 'YMinorTick'; 'YTickLabelRotation'; 'YTickLabelRotationMode';
  'ZAxis'; 'ZColorMode'; 'ZLimitMethod'; 'ZMinorTick'; 'ZTickLabelRotation';
  'ZTickLabelRotationMode'});
%=============================================================================
axes(ax);
l = line('Parent', ax, 'XData', 1:3, 'YData', 1:3);
assert_properties(l, common_interaction_properties());
assert_properties(l, {'AffectAutoLimits'; 'AlignVertexCenters'; 'ColorMode'; 'LineJoin';
  'LineStyleMode'; 'MarkerIndices'; 'MarkerMode'; 'SeriesIndex'});
%=============================================================================
eb = errorbar(ax, 1:3, [2 4 3], [0.2 0.3 0.4]);
assert_reference_properties(eb, reference_errorbar_properties(), ...
  {'Annotation'; 'BeingDeleted'; 'Children'; 'DataTipTemplate'; 'Parent'; 'Type'});
asserts.isequal(eb.Type, 'errorbar');
%=============================================================================
an = animatedline(ax, 1:3, [1 3 2], 'DisplayName', 'animated');
assert_reference_properties(an, reference_animatedline_properties(), ...
  {'BeingDeleted'; 'Children'; 'Parent'; 'Type'});
assert_properties(an, common_interaction_properties());
assert_properties(an, {'MaximumNumPoints'; 'ColorMode'; 'LineStyleMode'; 'MarkerMode';
  'SeriesIndex'});
%=============================================================================
s = scatter(1:3, [3 1 2], 36, [1 0 0], 'filled');
assert_properties(s, common_interaction_properties());
assert_properties(s, {'AlphaDataMapping'; 'AlphaDataMode'; 'AlphaVariable'; 'CDataSource';
  'ColorVariable'; 'DataTipTemplate'; 'SeriesIndex'; 'SizeDataMode'; 'SizeDataSource';
  'SizeVariable'; 'SourceTable'; 'XDataSource'; 'XJitter'; 'XJitterDirection';
  'XJitterWidth'; 'XVariable'; 'YDataMode'; 'YDataSource'; 'YJitter';
  'YJitterDirection'; 'YJitterWidth'; 'YVariable'; 'ZDataMode'; 'ZDataSource';
  'ZJitter'; 'ZJitterDirection'; 'ZJitterWidth'; 'ZVariable'});
%=============================================================================
hg = histogram(ax, [1 1 2 3], [0 2 4]);
assert_reference_properties(hg, reference_histogram_properties(), ...
  {'BeingDeleted'; 'Children'; 'Parent'; 'Type'; 'Values'});
asserts.istrue(isprop(hg, 'BinEdges'));
asserts.isequal(hg.Type, 'histogram');
%=============================================================================
hg2 = histogram2(ax, [1 1 2 3], [1 2 2 3], [0 2 4], [0 2 4]);
assert_reference_properties(hg2, reference_histogram2_properties(), ...
  {'Annotation'; 'BeingDeleted'; 'Children'; 'DataTipTemplate'; 'Parent'; 'Type'; 'Values'});
asserts.istrue(isprop(hg2, 'XBinEdges'));
asserts.isequal(hg2.Type, 'histogram2');
%=============================================================================
bc = boxchart(ax, [1 2 3; 4 5 6; 7 8 9]);
assert_reference_properties(bc, reference_boxchart_properties(), ...
  {'BeingDeleted'; 'Children'; 'DataTipTemplate'; 'Parent'; 'Type'});
asserts.isequal(bc.Type, 'boxchart');
%=============================================================================
z = peaks(8);
axes(ax);
sf = surf(z);
assert_properties(sf, common_interaction_properties());
assert_properties(sf, {'AlignVertexCenters'; 'CDataSource'; 'DataTipTemplate'; 'DisplayName';
  'FaceNormals'; 'FaceNormalsMode'; 'VertexNormalsMode'; 'XDataSource'; 'YDataSource';
  'ZDataSource'});
%=============================================================================
fs = fsurf(ax, @(x, y) x + y);
% FunctionSurface exposes a reduced introspection surface: the reference
% numerical-computing language does not expose Clipping, the *DataSource /
% FaceNormals* / VertexNormals* / AlignVertexCenters family for it, so those are
% not asserted here (they remain reachable through dotted access).
assert_properties(fs, {'Annotation'; 'ButtonDownFcn'; 'ContextMenu'; 'HandleVisibility';
  'HitTest'; 'PickableParts'; 'Selected'; 'SelectionHighlight'});
assert_properties(fs, {'Function'; 'MeshDensity'; 'ShowContours'; 'XRange'; 'XRangeMode';
  'YRange'; 'YRangeMode'; 'DataTipTemplate'; 'DisplayName'});
%=============================================================================
p = patch('Parent', ax, 'XData', [0 1 1 0], 'YData', [0 0 1 1], 'FaceColor', [0 0.5 1]);
assert_properties(p, common_interaction_properties());
assert_properties(p, {'AlignVertexCenters'; 'CDataMode'; 'FaceNormals'; 'FaceNormalsMode';
  'FaceVertexAlphaData'; 'FaceVertexCDataMode'; 'LineJoin'; 'SeriesIndex';
  'VertexNormalsMode'});
%=============================================================================
a = area(ax, [1 2 3; 2 1 4]);
assert_properties(a(1), common_interaction_properties());
assert_properties(a(1), {'BaseLine'; 'BaseValue'; 'DataTipTemplate'; 'FaceColorMode';
  'ShowBaseLine'; 'XDataSource'; 'YDataSource'});
asserts.isequal(a(1).Type, 'area');
%=============================================================================
b = bar(ax, [1 2; 3 4]);
assert_reference_properties(b(1), reference_bar_properties(), ...
  {'Annotation'; 'BaseLine'; 'BeingDeleted'; 'Children'; 'DataTipTemplate'; ...
  'Parent'; 'Type'; 'XEndPoints'; 'YEndPoints'});
asserts.isequal(b(1).Type, 'bar');
%=============================================================================
pch = piechart(f, [1 2 3]);
assert_reference_properties(pch, reference_piechart_properties(), ...
  {'CategoryCounts'; 'Parent'; 'Proportions'; 'Type'; 'WedgeDisplayData'; 'WedgeDisplayNames'});
asserts.isequal(pch.Type, 'piechart');
%=============================================================================
dch = donutchart(f, [1 2 3]);
assert_reference_properties(dch, reference_donutchart_properties(), ...
  {'CategoryCounts'; 'Parent'; 'Proportions'; 'Type'; 'WedgeDisplayData'; 'WedgeDisplayNames'});
asserts.isequal(dch.Type, 'donutchart');
%=============================================================================
axes(ax);
im = imagesc(magic(8));
% image does not expose Annotation in the reference introspection surface.
assert_properties(im, {'ButtonDownFcn'; 'Clipping'; 'ContextMenu'; 'HandleVisibility';
  'HitTest'; 'PickableParts'; 'Selected'; 'SelectionHighlight'});
assert_properties(im, {'Interpolation'; 'MaxRenderedResolution'});
%=============================================================================
axes(ax);
t = text(0.5, 0.5, 'sample');
assert_properties(t, text_interaction_properties());
assert_properties(t, {'AffectAutoLimits'; 'ColorMode'; 'Editing'; 'Interactions'; 'SeriesIndex'});
%=============================================================================
annLine = annotation(f, 'line');
assert_properties(annLine, {'Color'; 'LineStyle'; 'LineWidth'; 'Position'; 'Units'; 'X'; 'Y'});
annArrow = annotation(f, 'arrow');
assert_properties(annArrow, {'Color'; 'HeadLength'; 'HeadStyle'; 'HeadWidth'; 'LineStyle';
  'LineWidth'; 'Position'; 'Units'; 'X'; 'Y'});
annDoubleArrow = annotation(f, 'doublearrow');
assert_properties(annDoubleArrow, {'Color'; 'Head1Length'; 'Head1Style'; 'Head1Width';
  'Head2Length'; 'Head2Style'; 'Head2Width'; 'LineStyle'; 'LineWidth'; 'Position';
  'Units'; 'X'; 'Y'});
annTextArrow = annotation(f, 'textarrow');
assert_properties(annTextArrow, {'Color'; 'FontAngle'; 'FontName'; 'FontSize'; 'FontUnits';
  'FontWeight'; 'HeadLength'; 'HeadStyle'; 'HeadWidth'; 'HorizontalAlignment';
  'Interpreter'; 'LineStyle'; 'LineWidth'; 'Position'; 'String'; 'TextBackgroundColor';
  'TextColor'; 'TextEdgeColor'; 'TextLineWidth'; 'TextMargin'; 'TextRotation';
  'Units'; 'VerticalAlignment'; 'X'; 'Y'});
annRectangle = annotation(f, 'rectangle');
assert_properties(annRectangle, {'Color'; 'FaceAlpha'; 'FaceColor'; 'LineStyle'; 'LineWidth';
  'Position'; 'Rotation'; 'Units'});
annEllipse = annotation(f, 'ellipse');
assert_properties(annEllipse, {'Color'; 'FaceColor'; 'LineStyle'; 'LineWidth'; 'Position';
  'Rotation'; 'Units'});
annTextBox = annotation(f, 'textbox');
assert_properties(annTextBox, {'BackgroundColor'; 'Color'; 'EdgeColor'; 'FaceAlpha';
  'FitBoxToText'; 'FontAngle'; 'FontName'; 'FontSize'; 'FontUnits'; 'FontWeight';
  'HorizontalAlignment'; 'Interpreter'; 'LineStyle'; 'LineWidth'; 'Margin'; 'Position';
  'Rotation'; 'String'; 'Units'; 'VerticalAlignment'});
%=============================================================================
axes(ax);
[~, c] = contour(peaks(8));
assert_properties(c, common_interaction_properties());
assert_properties(c, {'DataTipTemplate'; 'XDataSource'; 'YDataSource'; 'ZDataSource'});
%=============================================================================
g = hggroup('Parent', ax);
assert_properties(g, {'Annotation'; 'ButtonDownFcn'; 'ContextMenu'; 'HandleVisibility';
  'HitTest'; 'PickableParts'; 'Selected'; 'SelectionHighlight'});
%=============================================================================
tl = tiledlayout(f, 1, 2);
assert_properties(tl, {'ContextMenu'; 'InnerPosition'});
%=============================================================================
sp = stackedplot((1:5)', [(1:5)' (2:6)']);
assert_properties(sp, {'HandleVisibility'; 'InnerPosition'; 'Layout'; 'OuterPosition';
  'Position'; 'PositionConstraint'; 'Units'});
asserts.isequal(sp.HandleVisibility, 'on');
asserts.isequal(sp.Units, 'normalized');
asserts.isequal(sp.Position, [0.13 0.11 0.775 0.815]);
asserts.isequal(sp.InnerPosition, [0.13 0.11 0.775 0.815]);
asserts.isequal(sp.OuterPosition, [0 0 1 1]);
asserts.isequal(sp.PositionConstraint, 'outerposition');
%=============================================================================
u = uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'ok');
assert_properties(u, {'ContextMenu'; 'Extent'; 'ForegroundColor'; 'InnerPosition';
  'OuterPosition'; 'Tag'});
u.ForegroundColor = [0.2 0.3 0.4];
asserts.isequal(u.ForegroundColor, [0.2 0.3 0.4]);
%=============================================================================
cm = uicontextmenu(f);
assert_reference_properties(cm, reference_uicontextmenu_properties(), ...
  {'BeingDeleted'; 'Children'; 'Parent'; 'Type'});
%=============================================================================
mn = uimenu(f, 'Text', 'Menu');
assert_reference_properties(mn, reference_uimenu_properties(), ...
  {'BeingDeleted'; 'Children'; 'Parent'; 'Type'});
%=============================================================================
f2 = figure('Visible', 'off');
ax2 = axes('Parent', f2);
hiddenLine = line('Parent', ax2, 'XData', 1:3, 'YData', 1:3);
hiddenLine.HandleVisibility = 'off';
asserts.istrue(isempty(findobj(f2, 'Type', 'line')));
root = groot();
root.ShowHiddenHandles = 'on';
asserts.istrue(any(findobj(f2, 'Type', 'line') == hiddenLine));
root.ShowHiddenHandles = 'off';
%=============================================================================
close all
%=============================================================================
function assert_properties(h, required)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
end
%=============================================================================
function assert_reference_properties(h, required, readonly)
  p = properties(h);
  asserts.istrue(all(ismember(required(:), p)));
  for k = 1:numel(required)
    name = required{k};
    value = get(h, name);
    if ~ismember(name, readonly)
      try
        set(h, name, value);
      catch
        error(['Property round-trip failed: ', name]);
      end
    end
  end
end
%=============================================================================
function names = reference_figure_properties()
  names = {'Position'; 'OuterPosition'; 'InnerPosition'; 'Units'; 'Renderer'; 'RendererMode';
    'Visible'; 'Color'; 'CloseRequestFcn'; 'CurrentAxes'; 'CurrentCharacter';
    'CurrentObject'; 'CurrentPoint'; 'SelectionType'; 'SizeChangedFcn'; 'FileName';
    'IntegerHandle'; 'NextPlot'; 'Alphamap'; 'Colormap'; 'GraphicsSmoothing';
    'WindowButtonDownFcn'; 'WindowButtonUpFcn'; 'WindowButtonMotionFcn';
    'WindowScrollWheelFcn'; 'WindowKeyPressFcn'; 'WindowKeyReleaseFcn'; 'MenuBar';
    'ToolBar'; 'Pointer'; 'PointerShapeCData'; 'PointerShapeHotSpot'; 'Name';
    'NumberTitle'; 'Icon'; 'Number'; 'Children'; 'Parent'; 'HandleVisibility';
    'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible';
    'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Clipping'; 'Scrollable';
    'WindowStyle'; 'WindowState'; 'DockControls'; 'Resize'; 'PaperPosition';
    'PaperPositionMode'; 'PaperSize'; 'PaperType'; 'PaperUnits'; 'InvertHardcopy';
    'PaperOrientation'; 'KeyPressFcn'; 'KeyReleaseFcn'; 'Theme'; 'ThemeMode';
    'ThemeChangedFcn'; 'AutoResizeChildren'};
end
%=============================================================================
function names = reference_axes_properties()
  names = {'CameraPosition'; 'CameraPositionMode'; 'CameraTarget'; 'CameraTargetMode';
    'CameraUpVector'; 'CameraUpVectorMode'; 'CameraViewAngle'; 'CameraViewAngleMode';
    'View'; 'Projection'; 'LabelFontSizeMultiplier'; 'AmbientLightColor';
    'DataAspectRatio'; 'DataAspectRatioMode'; 'PlotBoxAspectRatio';
    'PlotBoxAspectRatioMode'; 'FontName'; 'FontAngle'; 'FontWeight'; 'FontSmoothing';
    'TickLabelInterpreter'; 'XLim'; 'XLimMode'; 'YLim'; 'YLimMode'; 'ZLim'; 'ZLimMode';
    'XLimitMethod'; 'YLimitMethod'; 'ZLimitMethod'; 'XDir'; 'YDir'; 'ZDir'; 'CLim';
    'CLimMode'; 'ALim'; 'ALimMode'; 'Layer'; 'TickLength'; 'GridLineWidth';
    'GridLineWidthMode'; 'MinorGridLineWidth'; 'MinorGridLineWidthMode';
    'GridLineStyle'; 'MinorGridLineStyle'; 'XAxisLocation'; 'XColor'; 'XColorMode';
    'XTick'; 'XTickMode'; 'XTickLabelRotation'; 'XTickLabelRotationMode'; 'XLabel';
    'XScale'; 'XTickLabel'; 'XTickLabelMode'; 'XMinorTick'; 'YAxisLocation'; 'YColor';
    'YColorMode'; 'YTick'; 'YTickMode'; 'YTickLabelRotation'; 'YTickLabelRotationMode';
    'YLabel'; 'YScale'; 'YTickLabel'; 'YTickLabelMode'; 'YMinorTick'; 'ZColor';
    'ZColorMode'; 'ZTick'; 'ZTickMode'; 'ZTickLabelRotation'; 'ZTickLabelRotationMode';
    'ZLabel'; 'ZScale'; 'ZTickLabel'; 'ZTickLabelMode'; 'ZMinorTick'; 'BoxStyle';
    'LineWidth'; 'Color'; 'ClippingStyle'; 'CurrentPoint'; 'ColorScale'; 'AlphaScale';
    'Title'; 'TitleHorizontalAlignment'; 'Subtitle'; 'GridColor'; 'GridColorMode';
    'MinorGridColor'; 'MinorGridColorMode'; 'GridAlpha'; 'GridAlphaMode';
    'MinorGridAlpha'; 'MinorGridAlphaMode'; 'XAxis'; 'ZAxis'; 'Box'; 'TickDir';
    'TickDirMode'; 'XGrid'; 'XMinorGrid'; 'YGrid'; 'YMinorGrid'; 'ZGrid'; 'ZMinorGrid';
    'YAxis'; 'Units'; 'Position'; 'InnerPosition'; 'OuterPosition'; 'PositionConstraint';
    'TightInset'; 'Colormap'; 'Alphamap'; 'ColorOrder'; 'ColorOrderIndex';
    'LineStyleOrder'; 'LineStyleOrderIndex'; 'LineStyleCyclingMethod'; 'FontUnits';
    'FontSize'; 'FontSizeMode'; 'TitleFontWeight'; 'SubtitleFontWeight';
    'TitleFontSizeMultiplier'; 'SortMethod'; 'Clipping'; 'NextPlot'; 'Toolbar';
    'Interactions'; 'InteractionOptions'; 'Children'; 'Parent'; 'Visible';
    'HandleVisibility'; 'Layout'; 'NextSeriesIndex'; 'ButtonDownFcn'; 'ContextMenu';
    'BusyAction'; 'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type';
    'Tag'; 'UserData'; 'Selected'; 'SelectionHighlight'; 'HitTest'; 'PickableParts';
    'Legend'};
end
%=============================================================================
function names = reference_bar_properties()
  names = {'BarLayout'; 'BarWidth'; 'GroupWidth'; 'GroupWidthMode'; 'BaseLine'; ...
    'EdgeColor'; 'EdgeAlpha'; 'FaceColor'; 'FaceColorMode'; 'CData'; 'CDataMode'; ...
    'FaceAlpha'; 'Horizontal'; 'LineStyle'; 'LineWidth'; 'XEndPoints'; ...
    'YEndPoints'; 'Labels'; 'LabelLocation'; 'LabelLocationMode'; 'LabelColor'; ...
    'LabelColorMode'; 'FontName'; 'FontSize'; 'FontWeight'; 'FontAngle'; ...
    'Interpreter'; 'Clipping'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility'; ...
    'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible'; ...
    'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected'; ...
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'DisplayName'; 'Annotation'; ...
    'SeriesIndex'; 'XData'; 'XDataMode'; 'XDataSource'; 'YData'; 'YDataSource'; ...
    'DataTipTemplate'; 'BaseValue'; 'ShowBaseLine'};
end
%=============================================================================
function names = reference_animatedline_properties()
  names = {'MaximumNumPoints'; 'Color'; 'ColorMode'; 'LineStyle'; 'LineStyleMode';
    'LineWidth'; 'AlignVertexCenters'; 'Marker'; 'MarkerMode'; 'MarkerSize';
    'MarkerEdgeColor'; 'MarkerFaceColor'; 'Clipping'; 'Children'; 'Parent'; 'Visible';
    'HandleVisibility'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted';
    'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected';
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'DisplayName'; 'Annotation';
    'SeriesIndex'};
end
%=============================================================================
function names = reference_errorbar_properties()
  names = {'AlignVertexCenters'; 'CapSize'; 'Clipping'; 'Color'; 'ColorMode';
    'LineWidth'; 'LineStyle'; 'LineStyleMode'; 'Marker'; 'MarkerMode';
    'MarkerEdgeColor'; 'MarkerFaceColor'; 'MarkerSize'; 'XNegativeDelta';
    'XNegativeDeltaSource'; 'XPositiveDelta'; 'XPositiveDeltaSource';
    'YNegativeDelta'; 'YNegativeDeltaSource'; 'YPositiveDelta';
    'YPositiveDeltaSource'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility';
    'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible';
    'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Selected';
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'DisplayName'; 'Annotation';
    'SeriesIndex'; 'XData'; 'XDataMode'; 'XDataSource'; 'YData'; 'YDataSource';
    'DataTipTemplate'};
end
%=============================================================================
function names = reference_histogram_properties()
  names = {'Data'; 'BinCounts'; 'NumBins'; 'BinEdges'; 'BinWidth'; 'BinMethod';
    'BinLimits'; 'Normalization'; 'FaceColor'; 'EdgeColor'; 'LineWidth'; 'LineStyle';
    'BinCountsMode'; 'BinLimitsMode'; 'Orientation'; 'DisplayStyle'; 'FaceAlpha';
    'EdgeAlpha'; 'Values'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility';
    'DisplayName'; 'Annotation'; 'DataTipTemplate'; 'Selected'; 'SelectionHighlight';
    'HitTest'; 'PickableParts'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction';
    'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag';
    'UserData'; 'SeriesIndex'};
end
%=============================================================================
function names = reference_histogram2_properties()
  names = {'Data'; 'BinCounts'; 'NumBins'; 'XBinEdges'; 'YBinEdges'; 'BinWidth';
    'BinMethod'; 'XBinLimits'; 'YBinLimits'; 'Normalization'; 'FaceColor';
    'EdgeColor'; 'LineWidth'; 'LineStyle'; 'FaceLighting'; 'BinCountsMode';
    'XBinLimitsMode'; 'YBinLimitsMode'; 'DisplayStyle'; 'FaceAlpha'; 'EdgeAlpha';
    'ShowEmptyBins'; 'Values'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility';
    'DisplayName'; 'Annotation'; 'DataTipTemplate'; 'Selected'; 'SelectionHighlight';
    'HitTest'; 'PickableParts'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction';
    'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag';
    'UserData'; 'SeriesIndex'};
end
%=============================================================================
function names = reference_boxchart_properties()
  names = {'XData'; 'YData'; 'XDataMode'; 'YDataMode'; 'XVariable'; 'YVariable';
    'SourceTable'; 'BoxWidth'; 'BoxFaceColor'; 'BoxEdgeColor'; 'BoxMedianLineColor';
    'BoxFaceAlpha'; 'CapWidth'; 'WhiskerLineColor'; 'LineWidth';
    'WhiskerLineStyle'; 'MarkerColor'; 'MarkerSize'; 'MarkerStyle'; 'Clipping';
    'Orientation'; 'ColorGroupLayout'; 'Notch'; 'JitterOutliers'; 'ColorGroupWidth';
    'BoxFaceColorMode'; 'BoxEdgeColorMode'; 'BoxMedianLineColorMode';
    'CapWidthMode'; 'MarkerColorMode'; 'ColorGroupWidthMode'; 'Children'; 'Parent';
    'Visible'; 'HandleVisibility'; 'DisplayName'; 'Annotation'; 'Selected';
    'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'ButtonDownFcn'; 'ContextMenu';
    'BusyAction'; 'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type';
    'Tag'; 'UserData'; 'SeriesIndex'; 'DataTipTemplate'};
end
%=============================================================================
function names = reference_piechart_properties()
  names = {'Data'; 'Names'; 'DataVariable'; 'NamesVariable'; 'Labels'; 'LabelStyle';
    'StartAngle'; 'Direction'; 'ExplodedWedges'; 'ColorOrder'; 'FaceColor'; 'EdgeColor';
    'FaceAlpha'; 'LineWidth'; 'FontSize'; 'FontName'; 'FontColor'; 'Interpreter';
    'LegendVisible'; 'LegendTitle'; 'Title'; 'DisplayOrder'; 'NumDisplayWedges';
    'ShowOthers'; 'Proportions'; 'CategoryCounts'; 'WedgeDisplayData';
    'WedgeDisplayNames'; 'DataMode'; 'FontSizeMode'; 'LabelsMode'; 'LabelStyleMode';
    'NamesMode'; 'Parent'; 'Visible'; 'HandleVisibility'; 'Units'; 'OuterPosition';
    'InnerPosition'; 'Position'; 'PositionConstraint'; 'Layout'; 'SourceTable'; 'Type'};
end
%=============================================================================
function names = reference_donutchart_properties()
  names = {'Data'; 'Names'; 'DataVariable'; 'NamesVariable'; 'Labels'; 'LabelStyle';
    'StartAngle'; 'Direction'; 'ExplodedWedges'; 'ColorOrder'; 'FaceColor'; 'EdgeColor';
    'FaceAlpha'; 'LineWidth'; 'FontSize'; 'FontName'; 'FontColor'; 'Interpreter';
    'LegendVisible'; 'LegendTitle'; 'Title'; 'DisplayOrder'; 'NumDisplayWedges';
    'ShowOthers'; 'Proportions'; 'CategoryCounts'; 'WedgeDisplayData';
    'WedgeDisplayNames'; 'DataMode'; 'FontSizeMode'; 'LabelsMode'; 'LabelStyleMode';
    'NamesMode'; 'Parent'; 'Visible'; 'HandleVisibility'; 'Units'; 'OuterPosition';
    'InnerPosition'; 'Position'; 'PositionConstraint'; 'Layout'; 'SourceTable';
    'InnerRadius'; 'CenterLabel'; 'CenterLabelFontSize'; 'CenterLabelFontSizeMode'; 'Type'};
end
%=============================================================================
function names = reference_uicontextmenu_properties()
  names = {'ContextMenuOpeningFcn'; 'Children'; 'Parent'; 'HandleVisibility';
    'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible';
    'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Clipping'};
end
%=============================================================================
function names = reference_uimenu_properties()
  names = {'MenuSelectedFcn'; 'Tooltip'; 'Visible'; 'ForegroundColor'; 'Enable';
    'Separator'; 'Accelerator'; 'Checked'; 'Text'; 'Position'; 'Children'; 'Parent';
    'HandleVisibility'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted';
    'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'; 'Clipping'};
end
%=============================================================================
function names = common_interaction_properties()
  names = {'Annotation'; 'ButtonDownFcn'; 'Clipping'; 'ContextMenu'; 'HandleVisibility';
    'HitTest'; 'PickableParts'; 'Selected'; 'SelectionHighlight'};
end
%=============================================================================
function names = text_interaction_properties()
  names = {'ButtonDownFcn'; 'Clipping'; 'ContextMenu'; 'HandleVisibility';
    'HitTest'; 'PickableParts'; 'Selected'; 'SelectionHighlight'};
end
%=============================================================================
