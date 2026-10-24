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
%=============================================================================
% A figure that defines WindowScrollWheelFcn owns the scroll wheel: the web
% display list flags it so the panel keeps its built-in wheel zoom off. Other
% figures carry no flag and keep zooming.
f = figure('Visible', 'off');
ax = axes('Parent', f);
plot(ax, 1:5);
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isequal(numel(frame.rotatableAxes), 1);
assert_isfalse(isfield(frame, 'hasScrollWheelCallback'));

set(f, 'WindowScrollWheelFcn', @(source, event) disp('wheel'));
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_istrue(frame.hasScrollWheelCallback);
assert_isequal(numel(frame.rotatableAxes), 1);

set(f, 'WindowScrollWheelFcn', []);
drawnow();
frame = jsondecode(__web_display_list__(f));
assert_isfalse(isfield(frame, 'hasScrollWheelCallback'));
close(f);
%=============================================================================
