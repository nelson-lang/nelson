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
fig = uifigure('Visible', 'off', 'Position', [100 100 1050 700], 'Color', [0 0 0]);
fig.AutoResizeChildren = 'off';
root = uipanel(fig, 'Position', [0 0 1050 700], 'BorderType', 'none', ...
  'BackgroundColor', [0.2 0.2 0.2]);
bottom = uilabel(root, 'Text', '', 'Position', [0 0 1050 50], ...
  'BackgroundColor', [1 0 0]);
top = uilabel(root, 'Text', '', 'Position', [0 650 1050 50], ...
  'BackgroundColor', [0 1 0]);

fig.Position = [100 100 630 420];
root.Position = [0 0 630 420];
bottom.Position = [0 0 630 40];
top.Position = [0 380 630 40];
fig.Visible = 'on';
drawnow();
pause(0.1);
drawnow();

frame = getframe(fig).cdata;
close(fig);

% Tolerate display scaling and window-manager clamping: not exactly 630x420,
% but a real rendered window with a green top band, a red bottom band and no
% black gap from a missing component. Bands are measured relative to the frame.
assert_isequal(size(frame, 3), 3);
h = size(frame, 1);
w = size(frame, 2);
assert_istrue(h >= 250 && w >= 380);
red = double(frame(:, :, 1));
green = double(frame(:, :, 2));
blue = double(frame(:, :, 3));

band = max(1, round(0.12 * h));
topBand = (green(1:band, :) > 180) & (red(1:band, :) < 80) & (blue(1:band, :) < 80);
bottomBand = (red(h - band + 1:h, :) > 180) & (green(h - band + 1:h, :) < 80) & (blue(h - band + 1:h, :) < 80);
blackPixels = (red < 10) & (green < 10) & (blue < 10);

assert_istrue(nnz(topBand) > 0.5 * band * w);
assert_istrue(nnz(bottomBand) > 0.5 * band * w);
assert_istrue(nnz(blackPixels) < 0.002 * h * w);
%=============================================================================
