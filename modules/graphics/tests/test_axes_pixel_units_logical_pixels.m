%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% Pixel units of an axes are logical pixels, like the figure Position: at a
% display scale above 100 % the figure canvas is rendered at device
% resolution, so the painted axes box must grow with the device pixel ratio.
%=============================================================================
close all
%=============================================================================
for visible = {'off', 'on'}
  f = figure('Visible', visible{1}, 'Position', [100 100 400 300], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Units', 'pixels', 'XLim', [0 1], 'YLim', [0 1], 'Visible', 'off');
  ax.Position = [40 60 200 100];
  patch(ax, [0 1 1 0], [0 0 1 1], 'r', 'EdgeColor', 'none');
  drawnow();
  frame = getframe(f).cdata;
  ratio = size(frame, 2) / f.Position(3);
  [left, bottom, width, height] = red_box(frame);
  assert_close_pixels(width, 200 * ratio);
  assert_close_pixels(height, 100 * ratio);
  assert_close_pixels(left, 40 * ratio);
  assert_close_pixels(bottom, 60 * ratio);
  close(f);
end
%=============================================================================
fig = uifigure('Position', [200 200 400 300], 'Visible', 'off');
ax = uiaxes(fig, 'Position', [0 100 400 100], 'XLim', [0 400], 'YLim', [0 100]);
patch(ax, [0 200 200 0], [0 0 100 100], 'r', 'EdgeColor', 'none');
drawnow();
frame = getframe(fig).cdata;
ratio = size(frame, 2) / fig.Position(3);
[left, ~, width, ~] = red_box(frame);
% The uiaxes plot box is inset inside Position; the patch covers its left half.
viewportLeft = 10.9333329518636;
viewportWidth = 400 - viewportLeft - 3.60000006357827;
assert_close_pixels(left, viewportLeft * ratio);
assert_close_pixels(width, viewportWidth / 2 * ratio);
close(fig);
%=============================================================================
function [left, bottom, width, height] = red_box(frame)
  red = frame(:, :, 1) > 200 & frame(:, :, 2) < 60 & frame(:, :, 3) < 60;
  cols = find(any(red, 1));
  rows = find(any(red, 2));
  asserts.istrue(~isempty(cols));
  left = cols(1) - 1;
  width = cols(end) - cols(1) + 1;
  height = rows(end) - rows(1) + 1;
  bottom = size(frame, 1) - rows(end);
end
%=============================================================================
function assert_close_pixels(value, expected)
  asserts.istrue(abs(value - expected) <= 3, sprintf('%g vs expected %g', value, expected));
end
%=============================================================================
