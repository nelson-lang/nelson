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
probePath = getenv('NELSON_QT_WORKSPACE_PROBE');
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe and set NELSON_QT_WORKSPACE_PROBE.');
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
titleText = 'Qt workspace controls';
figureObject = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal', 'Position', [120 120 1000 650]);
ax = axes('Parent', figureObject, 'Position', [0.1 0.1 0.8 0.8]);
first = line('Parent', ax, 'XData', [0 5 10], 'YData', [8 8 8], 'DisplayName', 'Upper signal', 'Color', [1 0 0]);
second = line('Parent', ax, 'XData', [0 5 10], 'YData', [0 5 10], 'DisplayName', 'Diagonal', 'Color', [0 0 1]);
set(ax, 'XLim', [0 10], 'YLim', [0 10]);
for name = {'figurePropertiesAction', 'figureShowDetailsAction'}
  invoke(probe, titleText, struct('operation', 'action', 'name', name{1}));
end
settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureLegendToolbar')), int32(1));
firstHandle = __web_graphics_handle__(first, 'number');
secondHandle = __web_graphics_handle__(second, 'number');
toggleName = sprintf('figureObjectToggle_%.0f', secondHandle);
asserts.isequal(invoke(probe, titleText, struct('operation', 'checked', 'name', toggleName)), int32(1));
beforePick = getframe(figureObject);
pickCenter(probe, titleText, figureObject);
asserts.isequal(invoke(probe, titleText, struct('operation', 'marker', 'handle', secondHandle, 'x', 0.5, 'y', 0.5)), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'selectedObject', 'handle', secondHandle)), int32(1));
afterPick = getframe(figureObject);
asserts.isequal(afterPick.cdata, beforePick.cdata);
invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePointMarker')), int32(1));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureRotateAction'));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePointMarker')), int32(0));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureInspectAction'));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePointMarker')), int32(1));
invoke(probe, titleText, struct('operation', 'action', 'name', toggleName)); settle(probe, titleText);
asserts.isequal(char(second.Visible), 'off');
asserts.isequal(char(first.Visible), 'on');
asserts.isequal(invoke(probe, titleText, struct('operation', 'checked', 'name', toggleName)), int32(0));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePointMarker')), int32(0));
invoke(probe, titleText, struct('operation', 'action', 'name', toggleName)); settle(probe, titleText);
asserts.isequal(char(second.Visible), 'on');
second.Visible = 'off'; invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'checked', 'name', toggleName)), int32(0));
second.Visible = 'on';
second.DisplayName = 'Renamed diagonal';
invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'actionText', 'name', toggleName, 'value', 'Renamed diagonal')), int32(1));
view(ax, 12, 40); invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureAzimuthSlider')), int32(24));
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureElevationSlider')), int32(80));
invoke(probe, titleText, struct('operation', 'slider', 'name', 'figureAzimuthSlider', 'value', 60.5, 'down', true));
invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
asserts.isequal(ax.View, [12 40]);
invoke(probe, titleText, struct('operation', 'slider', 'name', 'figureAzimuthSlider', 'down', false));
settle(probe, titleText);
asserts.isapprox(ax.View, [60.5 40], 1e-8);
invoke(probe, titleText, struct('operation', 'slider', 'name', 'figureElevationSlider', 'value', -15.5));
settle(probe, titleText);
asserts.isapprox(ax.View, [60.5 -15.5], 1e-8);
invoke(probe, titleText, struct('operation', 'sliderKey', 'name', 'figureElevationSlider'));
settle(probe, titleText);
asserts.isapprox(ax.View, [60.5 -15], 1e-8);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePointMarker')), int32(0));
view(ax, 0, 90); invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePropertiesAction'));
settle(probe, titleText);
pickCenter(probe, titleText, figureObject);
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePropertiesAction'));
settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'selectedObject', 'handle', secondHandle)), int32(1));
otherAxes = axes('Parent', figureObject, 'Position', [0.1 0.1 0.8 0.8], 'Visible', 'off');
third = line('Parent', otherAxes, 'XData', [1 2 3], 'YData', [3 2 1], 'DisplayName', 'Other axes');
view(otherAxes, -20, 35);
figureObject.CurrentAxes = otherAxes;
invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
otherName = sprintf('figureObjectToggle_%.0f', __web_graphics_handle__(third, 'number'));
asserts.isequal(invoke(probe, titleText, struct('operation', 'checked', 'name', otherName)), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureAzimuthSlider', 'expected', -40)), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureElevationSlider')), int32(70));
invoke(probe, titleText, struct('operation', 'action', 'name', otherName)); settle(probe, titleText);
asserts.isequal(char(third.Visible), 'off');
asserts.isequal(char(second.Visible), 'on');
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePointMarker')), int32(0));
figureObject.CurrentAxes = ax;
invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
delete(first); invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'actionExists', 'name', sprintf('figureObjectToggle_%.0f', firstHandle))), int32(0));
pickCenter(probe, titleText, figureObject);
invoke(probe, titleText, struct('operation', 'snapshot', 'path', ...
  [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-figure-controls.png']));
clf(figureObject);
ax = axes('Parent', figureObject, 'Position', [0.1 0.1 0.8 0.8]);
thirdDimension = line('Parent', ax, 'XData', [-1 0 1], 'YData', [-1 0 1], 'ZData', [-1 0 1], 'DisplayName', '3D samples');
set(ax, 'XLim', [-1 1], 'YLim', [-1 1], 'ZLim', [-1 1]); view(ax, -37.5, 30);
invoke(probe, titleText, struct('operation', 'refresh')); settle(probe, titleText);
pickCenter(probe, titleText, figureObject);
thirdHandle = __web_graphics_handle__(thirdDimension, 'number');
asserts.isequal(invoke(probe, titleText, struct('operation', 'marker', 'handle', thirdHandle, 'x', 0.5, 'y', 0.5)), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'selectedObject', 'handle', thirdHandle)), int32(1));
close(figureObject);
dlclose(library);

function pickCenter(probe, titleText, figureObject)
  geometry = __figure_native_geometry__(figureObject);
  point = geometry(3:4) / 2;
  __figure_mouse_event__(figureObject, 'press', point);
  __figure_mouse_event__(figureObject, 'release', point);
  settle(probe, titleText);
end

function result = invoke(probe, titleText, request)
  result = dlcall(probe, titleText, jsonencode(request));
  asserts.istrue(result >= 0, sprintf('Qt probe failed: %s (%d)', jsonencode(request), result));
end

function settle(probe, titleText)
  for index = 1:300
    drawnow();
    pause(0.01);
    if index >= 15 && dlcall(probe, titleText, jsonencode(struct('operation', 'idle'))) == 1
      return;
    end
  end
  error('Qt figure controls did not settle.');
end
