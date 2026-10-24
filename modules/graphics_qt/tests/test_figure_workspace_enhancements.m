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
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe first.');
settingsPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-figure-enhancements.ini'];
settings = fopen(settingsPath, 'wt');
fprintf(settings, '[FigureWorkspace]\nnavigation=false\ndetails=false\nproperties=false\nwidth=320\n');
fclose(settings);
previousSettings = getenv('NELSON_QT_FIGURE_SETTINGS');
setenv('NELSON_QT_FIGURE_SETTINGS', settingsPath);
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
titleText = 'Qt workspace enhancements';
f = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal', 'Position', [100 100 1100 700]);
ax = axes('Parent', f, 'Position', [0.1 0.1 0.8 0.8]);
signal = line('Parent', ax, 'XData', [0 5 10], 'YData', [0 5 10], 'DisplayName', 'Signal');
other = line('Parent', ax, 'XData', [0 5 10], 'YData', [8 8 8], 'DisplayName', 'Other');
set(ax, 'XLim', [0 10], 'YLim', [0 10]);
settle(probe, titleText);
for name = {'figurePropertiesAction', 'figureShowNavigationAction', 'figureShowDetailsAction'}
  invoke(probe, titleText, struct('operation', 'action', 'name', name{1}));
end
settle(probe, titleText);
object = __web_graphics_handle__(signal, 'number');
invoke(probe, titleText, struct('operation', 'select', 'handle', object));
settle(probe, titleText);
invoke(probe, titleText, struct('operation', 'editor', 'name', 'LineStyle', 'handle', object, 'value', '--'));
settle(probe, titleText);
asserts.isequal(char(signal.LineStyle), '--');
invoke(probe, titleText, struct('operation', 'controlKey', 'key', 'Z', 'undo', true));
settle(probe, titleText);
asserts.isequal(char(signal.LineStyle), '-');
invoke(probe, titleText, struct('operation', 'controlKey', 'key', 'Z', 'redo', true));
settle(probe, titleText);
asserts.isequal(char(signal.LineStyle), '--');
invoke(probe, titleText, struct('operation', 'editor', 'name', 'Marker', 'handle', object, 'value', 'o'));
settle(probe, titleText);
asserts.isequal(char(signal.Marker), 'o');
invoke(probe, titleText, struct('operation', 'editor', 'name', 'MarkerSize', 'handle', object, 'value', 9));
settle(probe, titleText);
asserts.isequal(signal.MarkerSize, 9);
invoke(probe, titleText, struct('operation', 'editor', 'name', 'Color', 'handle', object, 'value', '[0,1,0]'));
settle(probe, titleText);
asserts.isequal(signal.Color, [0 1 0]);
name = sprintf('figureObjectToggle_%.0f', object);
invoke(probe, titleText, struct('operation', 'legendDoubleClick', 'name', name));
settle(probe, titleText);
asserts.isequal(char(signal.Visible), 'on');
asserts.isequal(char(other.Visible), 'off');
invoke(probe, titleText, struct('operation', 'legendDoubleClick', 'name', name));
settle(probe, titleText);
asserts.isequal(char(other.Visible), 'on');
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureInspectAction'));
geometry = __figure_native_geometry__(f); point = geometry(3:4) / 2;
__figure_mouse_event__(f, 'press', point); __figure_mouse_event__(f, 'release', point);
settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'textContains', 'name', 'figurePointTip', 'value', 'Signal')), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'textContains', 'name', 'figurePointTip', 'value', 'Point 2')), int32(1));
invoke(probe, titleText, struct('operation', 'controlKey', 'key', 'Right')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'textContains', 'name', 'figurePointTip', 'value', 'Point 3')), int32(1));
invoke(probe, titleText, struct('operation', 'controlKey', 'key', 'Left')); settle(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'textContains', 'name', 'figurePointTip', 'value', 'Point 2')), int32(1));
invoke(probe, titleText, struct('operation', 'dockWidth', 'value', 420));
pause(0.5); drawnow(); close(f); drawnow();
f = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal', 'Position', [100 100 1100 700]);
plot(1:3); settle(probe, titleText);
for name = {'figurePropertiesDock', 'figureNavigationToolbar', 'figureLegendToolbar'}
  asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', name{1})), int32(1));
end
width = invoke(probe, titleText, struct('operation', 'dockWidth'));
asserts.istrue(abs(double(width) - 420) < 12, sprintf('Restored width: %d', width));
close(f); dlclose(library);
setenv('NELSON_QT_FIGURE_SETTINGS', previousSettings);

function result = invoke(probe, titleText, request)
  result = dlcall(probe, titleText, jsonencode(request));
  asserts.istrue(result >= 0, sprintf('Qt enhancement failed: %s (%d)', jsonencode(request), result));
end

function settle(probe, titleText)
  dlcall(probe, titleText, jsonencode(struct('operation', 'refresh')));
  for index = 1:300
    drawnow(); pause(0.01);
    if index >= 20 && dlcall(probe, titleText, jsonencode(struct('operation', 'idle'))) == 1
      return;
    end
  end
  error('Qt workspace did not settle.');
end
