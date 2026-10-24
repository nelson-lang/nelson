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
titleText = 'Qt workspace integration';
figureObject = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal', 'Position', [120 120 1000 700]);
layout = tiledlayout(2, 2);
a1 = nexttile(); plot([1 10 100], [1 10 100]); title('Signal');
a2 = nexttile(); plot3([0 1 2], [0 1 0], [0 1 2]); title('Curve');
a3 = nexttile(); surf(peaks(15)); view(-52, 20); title('Surface');
a4 = nexttile(); plot(5:-1:1); title('Other');
drain(probe, titleText);
for name = {'figureInspectAction', 'figurezoomInAction', 'figurezoomOutAction', ...
    'figurefitAction', 'figurerestoreAction', 'figurereferenceAction'}
  asserts.isequal(invoke(probe, titleText, struct('operation', 'icon', 'name', name{1})), int32(1));
end
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureNavigationToolbar')), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figurePropertiesDock')), int32(0));
slots = [a1.Position; a2.Position; a3.Position; a4.Position];
limits = [a3.XLim; a3.YLim; a3.ZLim];
camera = a3.CameraPosition;
other = [a1.XLim a1.YLim a2.View a4.XLim];
invoke(probe, titleText, struct('operation', 'key', 'key', 'p'));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'checked', 'name', 'figurePanAction')), int32(1));
point = nativePoint(figureObject, a3, 0.5, 0.5);
beforeHover = getframe(figureObject);
__figure_mouse_event__(figureObject, 'motion', point); drain(probe, titleText);
afterHover = getframe(figureObject);
asserts.isequal(afterHover.cdata, beforeHover.cdata);
asserts.istrue(__figure_mouse_event__(figureObject, 'press', point));
drain(probe, titleText);
asserts.istrue(__figure_mouse_event__(figureObject, 'motion', point + [30 -15]));
drain(probe, titleText);
asserts.istrue(norm(a3.CameraPosition - camera) > 1e-8);
asserts.isequal(a3.View, [-52 20]);
asserts.isequal([a1.Position; a2.Position; a3.Position; a4.Position], slots);
asserts.isequal([a1.XLim a1.YLim a2.View a4.XLim], other);
asserts.isequal(invoke(probe, titleText, struct('operation', 'indicator', ...
  'handle', __web_graphics_handle__(a3, 'number'))), int32(1));
asserts.istrue(__figure_mouse_event__(figureObject, 'release', point + [30 -15]));
drain(probe, titleText);
asserts.isequal(figureObject.CurrentAxes, a3);
target = a3.CameraTarget;
invoke(probe, titleText, struct('operation', 'key', 'key', 'r'));
point = nativePoint(figureObject, a3, 0.5, 0.5);
__figure_mouse_event__(figureObject, 'press', point); drain(probe, titleText);
__figure_mouse_event__(figureObject, 'motion', point + [12 8]); drain(probe, titleText);
__figure_mouse_event__(figureObject, 'release', point + [12 8]); drain(probe, titleText);
asserts.isapprox(a3.CameraTarget, target, 1e-8);
asserts.istrue(norm(a3.View - [-52 20]) > 1);
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurerestoreAction'));
drain(probe, titleText);
asserts.isapprox([a3.XLim; a3.YLim; a3.ZLim], limits, 1e-8);
asserts.isapprox(a3.CameraPosition, camera, 1e-8);
invoke(probe, titleText, struct('operation', 'key', 'key', 'r'));
point = nativePoint(figureObject, a3, 0.5, 0.5);
__figure_mouse_event__(figureObject, 'press', point); drain(probe, titleText);
__figure_mouse_event__(figureObject, 'motion', point + [36 12]); drain(probe, titleText);
orientation = a3.View;
asserts.istrue(norm(orientation - [-52 20]) > 1);
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurefitAction'));
drain(probe, titleText);
__figure_mouse_event__(figureObject, 'motion', point + [72 24]);
__figure_mouse_event__(figureObject, 'release', point + [72 24]); drain(probe, titleText);
asserts.isapprox(a3.View, orientation, 1e-8);
asserts.isequal(a3.CameraViewAngleMode, 'auto');
angle = a3.CameraViewAngle;
invoke(probe, titleText, struct('operation', 'wheel', 'x', 0.25, 'y', 0.75, 'delta', 120));
drain(probe, titleText);
asserts.istrue(a3.CameraViewAngle < angle);
point = nativePoint(figureObject, a3, 0.5, 0.5);
__figure_mouse_event__(figureObject, 'press', point, 'native', 'open');
__figure_mouse_event__(figureObject, 'release', point); drain(probe, titleText);
asserts.isequal(a3.CameraViewAngleMode, 'auto');
figureObject.WindowKeyPressFcn = @(src, evt) set(src, 'UserData', evt.Key);
invoke(probe, titleText, struct('operation', 'key', 'key', 'p')); drain(probe, titleText);
asserts.istrue(strcmpi(figureObject.UserData, 'p'));
asserts.isequal(invoke(probe, titleText, struct('operation', 'checked', 'name', 'figureRotateAction')), int32(1));
figureObject.WindowKeyPressFcn = [];
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePropertiesAction')); drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'axes', 'handle', __web_graphics_handle__(a1, 'number'))); drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'property', 'name', 'XScale', 'value', 'log')); drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'property', 'name', 'XDir', 'value', 'reverse')); drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'property', 'name', 'XLim', 'value', '[1 100]')); drain(probe, titleText);
asserts.isequal(a1.XScale, 'log');
asserts.isequal(a1.XDir, 'reverse');
asserts.isequal(a1.XLim, [1 100]);
a1.XLim = [10 1000]; drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurezoomInAction')); drain(probe, titleText);
asserts.isapprox(log10(a1.XLim), [1.2 2.8], 1e-8);
set(a1, 'XLim', [1 100], 'YLim', [0 10]);
zoom(a1, 'on'); drain(probe, titleText);
start = nativePoint(figureObject, a1, 0.25, 0.25);
finish = nativePoint(figureObject, a1, 0.75, 0.75);
__figure_mouse_event__(figureObject, 'press', start);
__figure_mouse_event__(figureObject, 'motion', finish);
__figure_mouse_event__(figureObject, 'release', finish); drain(probe, titleText);
asserts.isapprox(log10(a1.XLim), [0.5 1.5], 0.03);
zoom(a1, 'off');
invoke(probe, titleText, struct('operation', 'select', 'handle', __web_graphics_handle__(a1.Title, 'number'))); drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'property', 'name', 'String', 'value', 'Edited in Qt')); drain(probe, titleText);
asserts.isequal(get(a1.Title, 'String'), 'Edited in Qt');
% The navigation bar is visible by default; the toggle hides then shows it.
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureShowNavigationAction')); drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureNavigationToolbar')), int32(0));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureShowNavigationAction')); drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureNavigationToolbar')), int32(1));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureDetails')), int32(0));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureShowDetailsAction')); drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureDetails')), int32(1));
outputPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-figure-saved.svg'];
invoke(probe, titleText, struct('operation', 'save', 'path', outputPath)); drain(probe, titleText);
if ~isfile(outputPath)
  invoke(probe, titleText, struct('operation', 'status'));
  error(['Saved image missing: ' outputPath]);
end
asserts.istrue(contains(fileread(outputPath), '<svg'));
% The main toolbar Export button is a distinct path from the Save as… menu;
% it must also produce a file (regressed when a cross-DLL pointer handoff
% passed the wrong base subobject and the export silently no-opped).
exportPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-figure-exported.png'];
if isfile(exportPath)
  rmfile(exportPath);
end
invoke(probe, titleText, struct('operation', 'export', 'path', exportPath)); drain(probe, titleText);
if ~isfile(exportPath)
  invoke(probe, titleText, struct('operation', 'status'));
  error(['Exported image missing: ' exportPath]);
end
asserts.istrue(dir(exportPath).bytes > 0);
beforePrint = figureObject.Color;
invoke(probe, titleText, struct('operation', 'print')); drain(probe, titleText);
asserts.isequal(figureObject.Color, beforePrint);
invoke(probe, titleText, struct('operation', 'snapshot', 'path', ...
  [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-figure-workspace.png']));
point = nativePoint(figureObject, a3, 0.5, 0.5);
__figure_mouse_event__(figureObject, 'press', point);
__figure_mouse_event__(figureObject, 'motion', point + [20 10]);
close(figureObject);
replacement = figure('Name', titleText, 'NumberTitle', 'off');
plot(1:5); replacementAxes = gca(); initial = replacementAxes.XLim;
drain(probe, titleText);
asserts.isequal(replacementAxes.XLim, initial);
close(replacement); drain(probe, titleText);
dlclose(library);

function result = invoke(probe, titleText, request)
  result = dlcall(probe, titleText, jsonencode(request));
  asserts.istrue(result >= 0, sprintf('Qt probe failed: %s (%d)', jsonencode(request), result));
end

function drain(probe, titleText)
  for index = 1:300
    drawnow();
    pause(0.01);
    ready = dlcall(probe, titleText, jsonencode(struct('operation', 'idle')));
    if index >= 3 && ready ~= 0
      return;
    end
  end
  invoke(probe, titleText, struct('operation', 'status'));
  error('Qt figure action did not settle.');
end

function point = nativePoint(figureObject, ax, fx, fy)
  frame = jsondecode(__web_display_list__(figureObject));
  boxes = frame.rotatableAxes;
  geometry = __figure_native_geometry__(figureObject);
  for index = 1:numel(boxes)
    if iscell(boxes)
      candidate = boxes{index};
    else
      candidate = boxes(index);
    end
    if candidate.handle == __web_graphics_handle__(ax, 'number')
      rect = candidate.rect;
      point = [(rect(1) + fx * rect(3)) * geometry(3), ...
        (1 - rect(2) - fy * rect(4)) * geometry(4)];
      return;
    end
  end
  error('No native subplot rectangle.');
end
