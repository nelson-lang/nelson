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
% A text uicontrol draws its String at the top, centred by default.
for visible = {'on', 'off'}
  [cx, cy, w, h] = inkCentre('center', visible{1});
  assert_istrue(abs(cx - w / 2) < w * 0.08);
  assert_istrue(cy < h / 2);
  cx = inkCentre('left', visible{1});
  assert_istrue(cx < w * 0.35);
  cx = inkCentre('right', visible{1});
  assert_istrue(cx > w * 0.65);
end
%=============================================================================
function [cx, cy, w, h] = inkCentre(alignment, visible)
  f = figure('Visible', visible, 'Position', [100 100 600 300], 'Color', [1 1 1]);
  uicontrol(f, 'Style', 'text', 'String', '9.2  32.2', 'FontSize', 26, ...
    'HorizontalAlignment', alignment, 'BackgroundColor', [1 1 1], ...
    'ForegroundColor', [0 0 0], 'Units', 'pixels', 'Position', [1 1 600 300]);
  drawnow();
  frame = getframe(f);
  close(f);
  ink = all(frame.cdata < 128, 3);
  [rows, cols] = find(ink);
  assert_isfalse(isempty(cols));
  [h, w] = size(ink);
  cx = mean(cols);
  cy = mean(rows);
end
%=============================================================================
