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
f = figure('Visible', 'off');
surf(peaks(30));
axis('off');
handle = __web_graphics_handle__(f, 'number');
ax = gca();
before = ax.View;
full = __web_display_list__(f);
request = struct('action', 'view', 'azimuth', 10, 'elevation', 20, ...
  'compactView', true, 'historyGroup', 'compact-drag');
profile('clear'); profile('on');
response = __web_figure_action__(handle, jsonencode(request));
profile('off');
stats = profile('info'); profile('clear');
asserts.isfalse(any(strcmp({stats.FunctionName}, 'drawnow')));
reply = jsondecode(response);
asserts.istrue(reply.state.canUndo);
asserts.isequal(ax.View, [10 20]);
request.azimuth = 30;
response = __web_figure_action__(handle, jsonencode(request));
asserts.istrue(numel(response) < numel(full));
request.compactView = false; request.softwareFallback = false;
reply = jsondecode(__web_figure_action__(handle, jsonencode(request)));
asserts.istrue(isfield(reply.displayList, 'webglMeshes'));
__web_figure_action__(handle, '{"action":"undo"}');
asserts.isapprox(ax.View, before, 1e-10);
__web_figure_action__(handle, '{"action":"redo"}');
asserts.isequal(ax.View, [30 20]);
close(f);
f = figure('Visible', 'off');
patch('Faces', [1 2 3], 'Vertices', [0 0 0; 1 0 1; 0 1 2]);
view(0, 90); axis('off');
frame = jsondecode(__web_display_list__(f));
asserts.istrue(frame.is3d);
asserts.istrue(frame.rotatableAxes(1).is3d);
close(f);
