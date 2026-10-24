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
% Annotation and text positions are logical pixels: at a display scale above
% 100 % the figure canvas is rendered at device resolution, so what they
% paint must grow with the device pixel ratio, drawn once, at the place the
% same position in normalized units gives.
%=============================================================================
close all
%=============================================================================
for visible = {'off', 'on'}
  f = figure('Visible', visible{1}, 'Position', [100 100 400 300], 'Color', [1 1 1]);
  axes(f, 'Visible', 'off');
  a = annotation(f, 'rectangle', [0.25 0.2 0.5 0.6], 'Color', 'r', 'LineWidth', 2);
  drawnow();
  frame = getframe(f).cdata;
  ratio = size(frame, 2) / f.Position(3);
  normalizedBox = red_box(frame);
  % the red box is the outer edge of the stroke: half a line width (points,
  % 1/72 inch) spills out on each side
  stroke = a.LineWidth * 96 / 72 * ratio;
  assert_close_pixels(normalizedBox, [100 60 200 180] * ratio + [-stroke / 2, -stroke / 2, stroke, stroke]);
  a.Units = 'pixels';
  fw = f.Position(3);
  fh = f.Position(4);
  pixelPosition = [0.25 * fw, 0.2 * fh, 0.5 * fw, 0.6 * fh];
  asserts.istrue(all(abs(a.Position - pixelPosition) <= 1));
  a.Position = pixelPosition;
  drawnow();
  assert_close_pixels(red_box(getframe(f).cdata), normalizedBox);
  close(f);
end
%=============================================================================
for visible = {'off', 'on'}
  f = figure('Visible', visible{1}, 'Position', [100 100 400 300], 'Color', [1 1 1]);
  ax = axes(f, 'Position', [0 0 1 1], 'Visible', 'off');
  t = text(ax, 0.25, 0.2, 'X', 'Units', 'normalized', 'Color', 'r', 'FontSize', 20);
  drawnow();
  normalizedBox = red_box(getframe(f).cdata);
  t.Units = 'pixels';
  t.Position = [100 60 0];
  drawnow();
  assert_close_pixels(red_box(getframe(f).cdata), normalizedBox);
  close(f);
end
%=============================================================================
function box = red_box(frame)
  red = frame(:, :, 1) > 200 & frame(:, :, 2) < 80 & frame(:, :, 3) < 80;
  cols = find(any(red, 1));
  rows = find(any(red, 2));
  asserts.istrue(~isempty(cols));
  box = [cols(1) - 1, size(frame, 1) - rows(end), cols(end) - cols(1) + 1, rows(end) - rows(1) + 1];
end
%=============================================================================
function assert_close_pixels(value, expected)
  asserts.istrue(all(abs(value - expected) <= 3));
end
%=============================================================================
