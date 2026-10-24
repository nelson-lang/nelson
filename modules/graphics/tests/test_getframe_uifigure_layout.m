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
fig = uifigure('Visible', 'off', 'Position', [100 100 630 420], 'Color', [0 0 0]);
fig.AutoResizeChildren = 'off';
root = uipanel(fig, 'Position', [0 0 630 420], 'BorderType', 'none', ...
  'BackgroundColor', [0.2 0.2 0.2]);
uilabel(root, 'Text', '', 'Position', [0 0 630 40], ...
  'BackgroundColor', [1 0 0]);
uilabel(root, 'Text', '', 'Position', [0 380 630 40], ...
  'BackgroundColor', [0 1 0]);
drawnow();

assert_complete_uifigure_frame(getframe(fig).cdata);

fig.Position = [140 130 630 420];
drawnow();
fig.Visible = 'on';
drawnow();
pause(0.1);
drawnow();

assert_complete_uifigure_frame(getframe(fig).cdata);
close(fig);
%=============================================================================
function assert_complete_uifigure_frame(image)
  % Tolerate display scaling and window-manager clamping: the captured frame
  % need not be exactly 630x420, only a real rendered window (not collapsed)
  % with a green band along the top, a red band along the bottom, and no black
  % gap left by a missing component. Bands are measured relative to the frame.
  h = size(image, 1);
  w = size(image, 2);
  assert_isequal(size(image, 3), 3);
  assert_istrue(h >= 250 && w >= 380);
  red = double(image(:, :, 1));
  green = double(image(:, :, 2));
  blue = double(image(:, :, 3));
  band = max(1, round(0.12 * h));
  topBand = (green(1:band, :) > 180) & (red(1:band, :) < 80) & (blue(1:band, :) < 80);
  bottomBand = (red(h - band + 1:h, :) > 180) & (green(h - band + 1:h, :) < 80) & (blue(h - band + 1:h, :) < 80);
  blackPixels = (red < 10) & (green < 10) & (blue < 10);
  assert_istrue(nnz(topBand) > 0.5 * band * w);
  assert_istrue(nnz(bottomBand) > 0.5 * band * w);
  assert_istrue(nnz(blackPixels) < 0.002 * h * w);
end
%=============================================================================
