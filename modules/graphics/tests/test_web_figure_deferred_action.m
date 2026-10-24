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
plot(1:5); old = gca();
handle = __web_graphics_handle__(f, 'number');
request = struct('action', 'view', 'axesHandle', __web_graphics_handle__(old, 'number'), ...
  'azimuth', 10, 'elevation', 20, 'historyGroup', 'old-gesture');
close(f);
__web_figure_deferred_action__(handle, jsonencode(request));
f = figure('Visible', 'off');
line = plot(1:5); ax = gca(); before = ax.View;
request.axesHandle = __web_graphics_handle__(line, 'number');
__web_figure_deferred_action__(handle, jsonencode(request));
asserts.isequal(ax.View, before);
other = figure('Visible', 'off');
plot(1:5);
request.axesHandle = __web_graphics_handle__(gca(), 'number');
__web_figure_deferred_action__(handle, jsonencode(request));
asserts.isequal(ax.View, before);
close(other);
handle = __web_graphics_handle__(f, 'number');
request.axesHandle = __web_graphics_handle__(ax, 'number');
__web_figure_deferred_action__(handle, jsonencode(request));
asserts.isequal(ax.View, [10 20]);
request = struct('action', 'zoom', 'axesHandle', request.axesHandle, 'factor', 0);
failed = false;
try
  __web_figure_deferred_action__(handle, jsonencode(request));
catch
  failed = true;
end
asserts.istrue(failed);
close(f);
