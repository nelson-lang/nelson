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
% The 'frame' style is a decorative filled, bordered rectangle.
fig = figure('Visible', 'off', 'Position', [100 100 200 120], 'Color', [1 1 1]);
u = uicontrol('Parent', fig, 'Style', 'frame', 'Position', [20 20 160 80]);
assert_isequal(get(u, 'Style'), 'frame');
%=============================================================================
% Defaults match the button-family styles (gray fill, dark-gray outline).
assert_isapprox(get(u, 'BackgroundColor'), [0.9608 0.9608 0.9608], 1e-3);
assert_isapprox(get(u, 'ForegroundColor'), [0.1294 0.1294 0.1294], 1e-3);
%=============================================================================
% A frame keeps its gray background (unlike edit/listbox/popupmenu which go
% white when their Style is established).
assert_isapprox(get(u, 'BackgroundColor'), [0.9608 0.9608 0.9608], 1e-3);
close(fig);
%=============================================================================
% The frame renders as a filled rectangle of its BackgroundColor.
if ~ismac()
  fig = figure('Visible', 'off', 'Position', [100 100 200 120], 'Color', [1 1 1]);
  uicontrol('Parent', fig, 'Style', 'frame', 'Position', [20 20 160 80], ...
    'BackgroundColor', [1 0 0]);
  drawnow();
  frame = getframe(fig);
  img = double(frame.cdata) / 255;
  redPixels = img(:, :, 1) > 0.6 & img(:, :, 2) < 0.35 & img(:, :, 3) < 0.35;
  assert_istrue(nnz(redPixels) > 0);
  close(fig);
end
%=============================================================================
