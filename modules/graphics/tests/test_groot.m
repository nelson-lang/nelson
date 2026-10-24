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
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
close all;
reset(groot());
g = groot();
%=============================================================================
expectedProperties = {'CallbackObject';
  'Children';
  'CurrentFigure';
  'FixedWidthFontName';
  'HandleVisibility';
  'MonitorPositions';
  'Parent';
  'PointerLocation';
  'ScreenDepth';
  'ScreenPixelsPerInch';
  'ScreenSize';
  'ShowHiddenHandles';
  'Tag';
  'Type';
  'Units';
  'UserData'};
asserts.isequal(sort(properties(g)), sort(expectedProperties));
%=============================================================================
asserts.isequal(g.Type, 'root');
asserts.isequal(g.Tag, '');
asserts.isequal(g.UserData, []);
asserts.isequal(g.Units, 'pixels');
asserts.isequal(g.FixedWidthFontName, 'Courier New');
asserts.isequal(g.HandleVisibility, 'on');
asserts.isequal(g.ShowHiddenHandles, 'off');
screenPixelsPerInch = g.ScreenPixelsPerInch;
asserts.istrue(isnumeric(screenPixelsPerInch));
asserts.istrue(screenPixelsPerInch > 0);
screenSizePixels = g.ScreenSize;
monitorPositionsPixels = g.MonitorPositions;
screenWidthPixels = screenSizePixels(3);
screenHeightPixels = screenSizePixels(4);
asserts.isequal(screenSizePixels(1:2), [1 1]);
asserts.istrue(screenWidthPixels > 0);
asserts.istrue(screenHeightPixels > 0);
asserts.isequal(monitorPositionsPixels, screenSizePixels);
asserts.istrue(isempty(g.Parent));
asserts.istrue(isempty(g.Children));
asserts.istrue(isempty(g.CurrentFigure));
asserts.istrue(isempty(g.CallbackObject));
asserts.isequal(size(g.PointerLocation), [1 2]);
asserts.istrue(isnumeric(g.ScreenDepth));
%=============================================================================
g.Tag = 'root-tag';
asserts.isequal(g.Tag, 'root-tag');
g.UserData = {'payload', 42};
asserts.isequal(g.UserData, {'payload', 42});
g.FixedWidthFontName = 'Consolas';
asserts.isequal(g.FixedWidthFontName, 'Consolas');
g.HandleVisibility = 'off';
asserts.isequal(g.HandleVisibility, 'off');
asserts.istrue(isgraphics(g));
g.ShowHiddenHandles = 'on';
asserts.isequal(g.ShowHiddenHandles, 'on');
g.Parent = [];
asserts.istrue(isempty(g.Parent));
%=============================================================================
depth = g.ScreenDepth;
g.ScreenDepth = 24;
asserts.isequal(g.ScreenDepth, 24);
g.ScreenPixelsPerInch = 100;
asserts.isequal(g.ScreenPixelsPerInch, screenPixelsPerInch);
reset(g);
asserts.isequal(g.ScreenDepth, depth);
asserts.isequal(g.FixedWidthFontName, 'Courier New');
asserts.isequal(g.ShowHiddenHandles, 'off');
%=============================================================================
g.Units = 'pixels';
dpi = g.ScreenPixelsPerInch;
asserts.isequal(g.ScreenSize, screenSizePixels);
asserts.isequal(g.MonitorPositions, monitorPositionsPixels);
g.Units = 'inches';
expectedInches = [0 0 screenWidthPixels / dpi screenHeightPixels / dpi];
asserts.isapprox(g.ScreenSize, expectedInches, 1e-12);
asserts.isapprox(g.MonitorPositions, expectedInches, 1e-12);
g.Units = 'centimeters';
expectedCentimeters = [0 0 screenWidthPixels * 2.54 / dpi screenHeightPixels * 2.54 / dpi];
asserts.isapprox(g.ScreenSize, expectedCentimeters, 1e-12);
asserts.isapprox(g.MonitorPositions, expectedCentimeters, 1e-12);
g.Units = 'points';
expectedPoints = [0 0 screenWidthPixels * 72 / dpi screenHeightPixels * 72 / dpi];
asserts.isapprox(g.ScreenSize, expectedPoints, 1e-12);
asserts.isapprox(g.MonitorPositions, expectedPoints, 1e-12);
g.Units = 'normalized';
asserts.isequal(g.ScreenSize, [0 0 1 1]);
asserts.isequal(g.MonitorPositions, [0 0 1 1]);
g.Units = 'characters';
expectedCharacters = [0 0 screenWidthPixels / (28 / 3) screenHeightPixels / 20];
asserts.isapprox(g.ScreenSize, expectedCharacters, 1e-12);
asserts.isapprox(g.MonitorPositions, expectedCharacters, 1e-12);
g.Units = 'pixels';
%=============================================================================
assert_readonly(g, 'CallbackObject', []);
assert_readonly(g, 'MonitorPositions', monitorPositionsPixels);
assert_readonly(g, 'ScreenSize', screenSizePixels);
assert_readonly(g, 'Type', 'root');
assert_fails(@() set(g, 'Parent', figure('Visible', 'off')));
close all;
reset(g);
%=============================================================================
global rootCallbackSource;
global rootCallbackObject;
global rootCallbackGcboObject;
rootCallbackSource = [];
rootCallbackObject = [];
rootCallbackGcboObject = [];
f = figure('Visible', 'off');
h = uicontrol('Parent', f, 'Style', 'pushbutton', 'CreateFcn', @storeRootCallbackState);
asserts.isequal(rootCallbackSource, h);
asserts.isequal(rootCallbackObject, h);
asserts.isequal(rootCallbackGcboObject, h);
asserts.istrue(isempty(g.CallbackObject));
clear global rootCallbackSource;
clear global rootCallbackObject;
clear global rootCallbackGcboObject;
close all;
reset(g);
%=============================================================================
f1 = figure('Visible', 'off', 'HandleVisibility', 'on');
f2 = figure('Visible', 'off', 'HandleVisibility', 'off');
asserts.isequal(numel(g.Children), 1);
asserts.isequal(g.Children, f1);
g.ShowHiddenHandles = 'on';
asserts.isequal(numel(g.Children), 2);
asserts.istrue(any(g.Children == f1));
asserts.istrue(any(g.Children == f2));
g.ShowHiddenHandles = 'off';
g.CurrentFigure = f1;
asserts.isequal(g.CurrentFigure, f1);
g.CurrentFigure = f2;
asserts.isequal(g.CurrentFigure, f1);
g.ShowHiddenHandles = 'on';
g.CurrentFigure = f2;
asserts.isequal(g.CurrentFigure, f2);
close all;
reset(g);
%=============================================================================
hiddenDefaults = {'DefaultFigureColormap'; 'DefaultFigureAlphamap'; 'DefaultAxesFontSize';
  'DefaultLineLineWidth'; 'DefaultTextInterpreter'; 'DefaultFigurePosition'};
visibleProperties = properties(g);
for k = 1:numel(hiddenDefaults)
  name = hiddenDefaults{k};
  asserts.isfalse(isprop(g, name));
  asserts.isfalse(any(strcmp(visibleProperties, name)));
  asserts.isfalse(isempty(get(g, name)));
end
asserts.isequal(size(get(g, 'DefaultFigureColormap')), [256 3]);
asserts.isequal(size(get(g, 'DefaultFigureAlphamap')), [1 64]);
asserts.isequal(get(g, 'DefaultAxesFontSize'), 10);
asserts.isequal(get(g, 'DefaultLineLineWidth'), 0.5);
asserts.isequal(get(g, 'DefaultTextInterpreter'), 'tex');
defaultPosition = expected_default_figure_position();
asserts.isequal(get(g, 'DefaultFigurePosition'), defaultPosition);
asserts.isequal(get(g, 'DefaultAxesXLim'), [0 1]);
%=============================================================================
set(g, 'DefaultAxesFontSize', 14);
f = figure('Visible', 'off');
ax = axes('Parent', f);
asserts.isequal(ax.FontSize, 14);
axExplicit = axes('Parent', f, 'FontSize', 9);
asserts.isequal(axExplicit.FontSize, 9);
set(g, 'DefaultAxesFontSize', 'remove');
axFactory = axes('Parent', f);
asserts.isequal(axFactory.FontSize, 10);
set(g, 'DefaultAxesXLim', [-2 2]);
axLimitsDefault = axes('Parent', f);
asserts.isequal(axLimitsDefault.XLim, [-2 2]);
set(g, 'DefaultAxesXLim', 'remove');
axLimitsFactory = axes('Parent', f);
asserts.isequal(axLimitsFactory.XLim, [0 1]);
%=============================================================================
set(g, 'DefaultLineLineWidth', 3);
lineDefault = line('Parent', ax, 'XData', 1:3, 'YData', 1:3);
asserts.isequal(lineDefault.LineWidth, 3);
lineExplicit = line('Parent', ax, 'XData', 1:3, 'YData', 3:-1:1, 'LineWidth', 1);
asserts.isequal(lineExplicit.LineWidth, 1);
set(g, 'DefaultLineLineWidth', 'remove');
lineFactory = line('Parent', ax, 'XData', 1:3, 'YData', [1 2 1]);
asserts.isequal(lineFactory.LineWidth, 0.5);
%=============================================================================
set(g, 'DefaultTextInterpreter', 'none');
textDefault = text(ax, 1, 1, 'value');
asserts.isequal(textDefault.Interpreter, 'none');
textExplicit = text(ax, 1, 2, 'value', 'Interpreter', 'tex');
asserts.isequal(textExplicit.Interpreter, 'tex');
set(g, 'DefaultTextInterpreter', 'remove');
textFactory = text(ax, 1, 3, 'value');
asserts.isequal(textFactory.Interpreter, 'tex');
%=============================================================================
set(g, 'DefaultFigurePosition', [100 100 320 240]);
fDefault = figure('Visible', 'off');
asserts.isequal(fDefault.Position, [100 100 320 240]);
fExplicit = figure('Visible', 'off', 'Position', [120 130 340 260]);
asserts.isequal(fExplicit.Position, [120 130 340 260]);
set(g, 'DefaultFigurePosition', 'remove');
factoryPosition = expected_default_figure_position();
asserts.isequal(get(g, 'DefaultFigurePosition'), factoryPosition);
fFactory = figure('Visible', 'off');
asserts.isequal(fFactory.Position, factoryPosition);
%=============================================================================
customMap = repmat([1 0 0], 4, 1);
set(g, 'DefaultFigureColormap', customMap);
fMap = figure('Visible', 'off');
asserts.isequal(fMap.Colormap, customMap);
set(g, 'DefaultFigureColormap', 'remove');
asserts.isequal(size(get(g, 'DefaultFigureColormap')), [256 3]);
customAlpha = [0 0.5 1];
set(g, 'DefaultFigureAlphamap', customAlpha);
fAlpha = figure('Visible', 'off');
asserts.isequal(fAlpha.Alphamap, customAlpha);
set(g, 'DefaultFigureAlphamap', 'remove');
asserts.isequal(size(get(g, 'DefaultFigureAlphamap')), [1 64]);
close all;
reset(g);
%=============================================================================
function assert_readonly(h, name, attemptedValue)
  failed = false;
  try
    set(h, name, attemptedValue);
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
%=============================================================================
function assert_fails(callback)
  failed = false;
  try
    callback();
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
%=============================================================================
function position = expected_default_figure_position()
  position = groot().DefaultFigurePosition;
end
%=============================================================================
