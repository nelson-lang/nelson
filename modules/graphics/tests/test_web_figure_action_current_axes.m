%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% Navigation actions must remain safe when a figure reports more than one
% current axes while a subplot layout is being refreshed.
f = figure('Visible', 'off');
subplot(1, 2, 1, 'Parent', f);
plot(1:5);
subplot(1, 2, 2, 'Parent', f);
plot(5:-1:1);
drawnow();
handle = __web_graphics_handle__(f, 'number');
axesHandle = __web_graphics_handle__(gca(), 'number');
request = struct('action', 'zoom', 'axesHandle', axesHandle, ...
  'factor', 1.1, 'compactView', true);
reply = jsondecode(__web_figure_action__(handle, jsonencode(request), false));
asserts.hasField(reply, 'state');
asserts.hasField(reply, 'displayList');
close(f);
