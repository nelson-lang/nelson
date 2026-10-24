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
close all
%=============================================================================
figfile = [tempname(), '.fig'];
%=============================================================================
figs = [localAxesObjectFigure(); localChartObjectFigure(); localUIObjectFigure(); ...
  localAnnotationFigure(); localPolarFigure(); localLayoutFigure(); localStackedPlotFigure()];
savefig(figs, figfile);
asserts.istrue(isfile(figfile));
localCloseFigures(figs);
%=============================================================================
restored = openfig(figfile, 'invisible');
asserts.isequal(numel(restored), 7);
%=============================================================================
localAssertTypeCount(restored(1), 'figure', 1);
localAssertTypeAtLeast(restored(1), 'axes', 1);
localAssertTypeAtLeast(restored(1), 'line', 2);
localAssertTypeAtLeast(restored(1), 'animatedline', 1);
localAssertTypeAtLeast(restored(1), 'area', 1);
localAssertTypeAtLeast(restored(1), 'bar', 1);
localAssertTypeAtLeast(restored(1), 'colorbar', 1);
localAssertTypeAtLeast(restored(1), 'contour', 1);
localAssertTypeAtLeast(restored(1), 'errorbar', 1);
localAssertTypeAtLeast(restored(1), 'functionsurface', 1);
localAssertTypeAtLeast(restored(1), 'hggroup', 1);
localAssertTypeAtLeast(restored(1), 'histogram', 1);
localAssertTypeAtLeast(restored(1), 'histogram2', 1);
localAssertTypeAtLeast(restored(1), 'image', 1);
localAssertTypeAtLeast(restored(1), 'legend', 1);
localAssertTypeAtLeast(restored(1), 'light', 1);
localAssertTypeAtLeast(restored(1), 'patch', 1);
localAssertTypeAtLeast(restored(1), 'quiver', 1);
localAssertTypeAtLeast(restored(1), 'scatter', 1);
localAssertTypeAtLeast(restored(1), 'surface', 1);
localAssertTypeAtLeast(restored(1), 'text', 1);
%=============================================================================
localAssertTypeAtLeast(restored(2), 'binscatter', 1);
localAssertTypeAtLeast(restored(2), 'boxchart', 1);
localAssertTypeAtLeast(restored(2), 'bubblechart', 1);
localAssertTypeAtLeast(restored(2), 'bubblelegend', 1);
localAssertTypeAtLeast(restored(2), 'donutchart', 1);
localAssertTypeAtLeast(restored(2), 'piechart', 1);
%=============================================================================
localAssertTypeAtLeast(restored(3), 'uicontextmenu', 1);
localAssertTypeAtLeast(restored(3), 'uicontrol', 1);
localAssertTypeAtLeast(restored(3), 'uimenu', 3);
%=============================================================================
localAssertTypeAtLeast(restored(4), 'lineshape', 1);
localAssertTypeAtLeast(restored(4), 'arrowshape', 1);
localAssertTypeAtLeast(restored(4), 'doubleendarrowshape', 1);
localAssertTypeAtLeast(restored(4), 'textarrowshape', 1);
localAssertTypeAtLeast(restored(4), 'rectangleshape', 1);
localAssertTypeAtLeast(restored(4), 'ellipseshape', 1);
localAssertTypeAtLeast(restored(4), 'textboxshape', 1);
%=============================================================================
localAssertTypeAtLeast(restored(5), 'polaraxes', 1);
localAssertTypeAtLeast(restored(5), 'line', 1);
%=============================================================================
localAssertTypeAtLeast(restored(6), 'tiledlayout', 1);
localAssertTypeAtLeast(restored(6), 'axes', 2);
%=============================================================================
localAssertTypeAtLeast(restored(7), 'StackedLineChart', 1);
%=============================================================================
localCloseFigures(restored);
if isfile(figfile)
  rmfile(figfile);
end
%=============================================================================
function f = localAxesObjectFigure()
  f = figure('Visible', 'off', 'Tag', 'axesObjectsFigure');
  ax = axes('Parent', f, 'Tag', 'axesObjectsAxes');
  hold(ax, 'on');

  __line__('Parent', ax, 'XData', [0 1 2], 'YData', [0 1 0], 'Tag', 'axesObjectsLine');
  animatedline(ax, [0 1 2], [1 2 1], 'Tag', 'axesObjectsAnimatedLine');
  area(ax, [0 1 2], [1 3 2], 'Tag', 'axesObjectsArea');
  bar(ax, [1 2 3], [3 1 2], 'Tag', 'axesObjectsBar');
  colorbar(ax);

  [X, Y, Z] = peaks(4);
  contour(ax, X, Y, Z, 'Tag', 'axesObjectsContour');
  errorbar(ax, [1 2 3], [2 4 3], [0.2 0.3 0.4], 'Tag', 'axesObjectsErrorBar');
  fsurf(ax, @(x, y) sin(x) + cos(y), [-2 2 -2 2], 'MeshDensity', 5, ...
    'Tag', 'axesObjectsFunctionSurface');
  histogram(ax, [1 1 2 3], [0 2 4], 'Tag', 'axesObjectsHistogram');
  histogram2(ax, [1 1 2 3], [1 2 2 3], [0 2 4], [0 2 4], ...
    'Tag', 'axesObjectsHistogram2');
  image(ax, 'CData', uint8([1 2; 3 4]), 'Tag', 'axesObjectsImage');
  legend(ax, 'sourceLine');
  light(ax, 'Tag', 'axesObjectsLight');
  patch(ax, 'XData', [0 1 0], 'YData', [0 0 1], 'FaceColor', [0 1 0], ...
    'Tag', 'axesObjectsPatch');
  quiver(ax, [0 1], [0 1], [1 0], [0 1], 'Tag', 'axesObjectsQuiver');
  scatter(ax, [1 2 3], [1 4 9], 36, [1 0 0], 'Tag', 'axesObjectsScatter');
  surface(ax, X, Y, Z, 'Tag', 'axesObjectsSurface');
  text(ax, 0.25, 0.75, 'axes objects text', 'Tag', 'axesObjectsText');

  group = hggroup(ax, 'Tag', 'axesObjectsGroup');
  __line__('Parent', group, 'XData', [0 1], 'YData', [1 0], 'Tag', 'axesObjectsGroupLine');
end
%=============================================================================
function f = localChartObjectFigure()
  f = figure('Visible', 'off', 'Tag', 'chartObjectsFigure');
  ax = axes('Parent', f, 'Tag', 'chartObjectsAxes');
  hold(ax, 'on');

  binscatter(ax, [0.1 0.2 0.8 1.2 1.8 1.9], [1 2 1 2 1 2], ...
    'NumBins', [2 2], 'Tag', 'chartObjectsBinscatter');
  boxchart(ax, [1 2 3 4 20], 'Tag', 'chartObjectsBoxChart');
  bubblechart(ax, 1:3, [3 2 1], [20 40 60], 'Tag', 'chartObjectsBubbleChart');
  bubblelegend(ax, 'Bubbles');
  piechart(f, [1 2 3]);
  donutchart(f, [1 2 3]);
end
%=============================================================================
function f = localUIObjectFigure()
  f = figure('Visible', 'off', 'Tag', 'uiObjectsFigure');
  cm = uicontextmenu(f, 'Tag', 'uiObjectsContextMenu');
  uimenu(cm, 'Text', 'context item', 'Tag', 'uiObjectsContextMenuItem');
  uimenu(f, 'Text', 'file menu', 'Tag', 'uiObjectsFileMenu');
  uimenu(f, 'Text', 'edit menu', 'Tag', 'uiObjectsEditMenu');
  uicontrol(f, 'Style', 'pushbutton', 'String', 'button', 'Tag', 'uiObjectsControl', ...
    'Position', [10 10 80 24]);
end
%=============================================================================
function f = localAnnotationFigure()
  f = figure('Visible', 'off', 'Tag', 'annotationObjectsFigure');
  annotation(f, 'line', [0.05 0.20], [0.95 0.95], 'Tag', 'annotationObjectsLine');
  annotation(f, 'arrow', [0.05 0.20], [0.85 0.85], 'Tag', 'annotationObjectsArrow');
  annotation(f, 'doublearrow', [0.05 0.20], [0.75 0.75], ...
    'Tag', 'annotationObjectsDoubleArrow');
  annotation(f, 'textarrow', [0.05 0.20], [0.65 0.65], 'String', 'text arrow', ...
    'Tag', 'annotationObjectsTextArrow');
  annotation(f, 'rectangle', [0.30 0.80 0.10 0.10], 'Tag', 'annotationObjectsRectangle');
  annotation(f, 'ellipse', [0.45 0.80 0.10 0.10], 'Tag', 'annotationObjectsEllipse');
  annotation(f, 'textbox', [0.60 0.78 0.20 0.12], 'String', 'textbox', ...
    'Tag', 'annotationObjectsTextbox');
end
%=============================================================================
function f = localLayoutFigure()
  f = figure('Visible', 'off', 'Tag', 'layoutFigure');
  t = tiledlayout(f, 1, 2, 'Tag', 'layoutObject');
  ax1 = nexttile(t);
  plot(ax1, 1:3, [1 4 9]);
  ax2 = nexttile(t);
  scatter(ax2, 1:3, [3 2 1]);
end
%=============================================================================
function f = localPolarFigure()
  f = figure('Visible', 'off', 'Tag', 'polarFigure');
  ax = polaraxes('Parent', f, 'Tag', 'polarAxesObject');
  polarplot(ax, [0 pi / 2 pi], [1 2 1], 'Tag', 'polarLineObject');
end
%=============================================================================
function f = localStackedPlotFigure()
  f = figure('Visible', 'off', 'Tag', 'stackedPlotFigure');
  temperature = [19.4; 20.1; 21.7; 23.2];
  pressure = [1012; 1011; 1009; 1008];
  rowTimes = datetime(2026, 1, 1) + hours(0:3)';
  T = table(rowTimes, temperature, pressure, ...
    'VariableNames', {'Time', 'Temperature', 'Pressure'});
  stackedplot(f, T);
end
%=============================================================================
function localAssertTypeCount(parent, typeName, expected)
  objects = findobj(parent, 'Type', typeName);
  asserts.isequal(numel(objects), expected);
end
%=============================================================================
function localAssertTypeAtLeast(parent, typeName, expected)
  objects = findobj(parent, 'Type', typeName);
  asserts.istrue(numel(objects) >= expected);
end
%=============================================================================
function localCloseFigures(figs)
  for k = 1:numel(figs)
    if isgraphics(figs(k), 'figure')
      close(figs(k));
    end
  end
end
%=============================================================================
