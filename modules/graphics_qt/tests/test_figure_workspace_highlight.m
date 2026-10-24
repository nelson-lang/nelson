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
skip_testsuite(isempty(probePath) || ~isfile(probePath), ...
  'Build the Qt workspace probe and set NELSON_QT_WORKSPACE_PROBE.');
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
titleText = 'Qt workspace highlight';
figureObject = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal', 'Position', [120 120 900 600]);
left = subplot(1, 2, 1); plot(1:3); title('Left');
right = subplot(1, 2, 2); plot(3:-1:1); title('Right');
drain(probe, titleText);
leftHandle = __web_graphics_handle__(left, 'number');
rightHandle = __web_graphics_handle__(right, 'number');
% Hovering an axes frames it by default.
__figure_mouse_event__(figureObject, 'motion', nativePoint(figureObject, left, 0.5, 0.5));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'indicator', 'handle', leftHandle)), int32(1));
% SelectionHighlight 'off' on one axes hides its frame only.
left.SelectionHighlight = 'off';
__figure_mouse_event__(figureObject, 'motion', nativePoint(figureObject, left, 0.4, 0.4));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'indicator', 'handle', leftHandle)), int32(0));
__figure_mouse_event__(figureObject, 'motion', nativePoint(figureObject, right, 0.5, 0.5));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'indicator', 'handle', rightHandle)), int32(1));
% SelectionHighlight 'off' on the figure hides every frame.
figureObject.SelectionHighlight = 'off';
__figure_mouse_event__(figureObject, 'motion', nativePoint(figureObject, right, 0.4, 0.4));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'indicator', 'handle', rightHandle)), int32(0));
% Gestures stay available: dragging in pan mode still moves the axes limits.
invoke(probe, titleText, struct('operation', 'key', 'key', 'p'));
drain(probe, titleText);
limits = right.XLim;
point = nativePoint(figureObject, right, 0.5, 0.5);
asserts.istrue(__figure_mouse_event__(figureObject, 'press', point));
drain(probe, titleText);
asserts.istrue(__figure_mouse_event__(figureObject, 'motion', point + [40 0]));
drain(probe, titleText);
asserts.istrue(__figure_mouse_event__(figureObject, 'release', point + [40 0]));
drain(probe, titleText);
asserts.istrue(any(abs(right.XLim - limits) > 1e-8));
% Turning the figure back on restores the frame of the axes left 'on'.
figureObject.SelectionHighlight = 'on';
__figure_mouse_event__(figureObject, 'motion', nativePoint(figureObject, right, 0.5, 0.5));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'indicator', 'handle', rightHandle)), int32(1));
close(figureObject); drain(probe, titleText);
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
