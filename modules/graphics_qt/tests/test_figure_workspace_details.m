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
titleText = 'Qt workspace details';
figureObject = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal');
surf(peaks(8)); view(-52, 20);
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureShowDetailsAction'));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureDetails')), int32(1));
assertText(probe, titleText, 'Inspect');
assertText(probe, titleText, 'Az -52.0');
assertText(probe, titleText, 'El 20.0');
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureRotateAction'));
invoke(probe, titleText, struct('operation', 'refresh')); drain(probe, titleText);
assertText(probe, titleText, 'Rotate');
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePanAction'));
invoke(probe, titleText, struct('operation', 'refresh')); drain(probe, titleText);
assertText(probe, titleText, 'Pan');
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureInspectAction'));
clf(figureObject);
ax = axes('Parent', figureObject, 'Position', [0.1 0.1 0.8 0.8]);
plot(ax, [0 5 10], [0 5 10]); set(ax, 'XLim', [0 10], 'YLim', [0 10]);
invoke(probe, titleText, struct('operation', 'refresh')); drain(probe, titleText);
geometry = __figure_native_geometry__(figureObject);
point = geometry(3:4) / 2;
__figure_mouse_event__(figureObject, 'press', point);
__figure_mouse_event__(figureObject, 'release', point); drain(probe, titleText);
assertText(probe, titleText, 'Point 2');
assertText(probe, titleText, 'X 5');
assertText(probe, titleText, 'Y 5');
invoke(probe, titleText, struct('operation', 'refresh')); drain(probe, titleText);
assertText(probe, titleText, 'Point 2');
invoke(probe, titleText, struct('operation', 'action', 'name', 'figureShowDetailsAction'));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureDetails')), int32(0));
asserts.isequal(invoke(probe, titleText, struct('operation', 'visible', 'name', 'figureNavigationToolbar')), int32(1));
close(figureObject);
dlclose(library);

function assertText(probe, titleText, value)
  asserts.isequal(invoke(probe, titleText, struct('operation', 'textContains', 'name', 'figureDetails', 'value', value)), int32(1));
end

function result = invoke(probe, titleText, request)
  result = dlcall(probe, titleText, jsonencode(request));
  asserts.istrue(result >= 0, sprintf('Qt probe failed: %s (%d)', jsonencode(request), result));
end

function drain(probe, titleText)
  for index = 1:300
    drawnow();
    pause(0.01);
    if index >= 3 && dlcall(probe, titleText, jsonencode(struct('operation', 'idle'))) == 1
      return;
    end
  end
  error('Qt figure action did not settle.');
end
