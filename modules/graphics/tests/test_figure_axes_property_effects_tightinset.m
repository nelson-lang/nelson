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
close all
%=============================================================================
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420], 'Color', [1 1 1]);
ax = axes('Parent', f);
set(ax, 'XLim', [0 1], 'YLim', [0 1], 'XTick', 0:0.25:1, 'YTick', 0:0.25:1, ...
  'Box', 'off', 'TickDir', 'in');
drawnow();
labelFrame = getframe(f).cdata;
labelPixels = tick_label_dark_pixel_count(labelFrame, ax);
% Thresholds recalibrated after the 1.25x font scale was removed (text now
% matches expected behavior), which lowered the tick-label pixel count on every
% platform (e.g. Windows/DirectWrite ~480, was above 650 with the oversized font).
if ispc()
  asserts.istrue(labelPixels > 380);
  asserts.istrue(labelPixels < 2000);
elseif ismac()
  asserts.istrue(labelPixels > 240);
else
  % FreeType renders thinner glyph edges than DirectWrite, so fewer pixels
  % fall below the darkness threshold for the same font metrics.
  asserts.istrue(labelPixels > 380);
end
close(f);
%=============================================================================
function assert_mode_switch(h, propertyName, modeName, value)
  set(h, modeName, 'auto');
  set(h, propertyName, value);
  asserts.isequal(get(h, modeName), 'manual');
end

%=============================================================================
function delta = frame_delta(a, b)
  delta = sum(abs(double(a(:)) - double(b(:))));
end
%=============================================================================
function frameSize = expectedFrameSize(f)
  position = f.Position;
  ratio = f.DevicePixelRatio;
  frameSize = [round(position(4) * ratio), round(position(3) * ratio), 3];
end
%=============================================================================
function [left, top, right, bottom] = axes_edges(frame, ax)
  imageHeight = size(frame, 1);
  imageWidth = size(frame, 2);
  position = ax.Position;
  left = max(3, round(position(1) * imageWidth));
  right = min(imageWidth - 2, round((position(1) + position(3)) * imageWidth));
  top = max(3, round((1 - position(2) - position(4)) * imageHeight));
  bottom = min(imageHeight - 2, round((1 - position(2)) * imageHeight));
end
%=============================================================================
function ratio = dark_ratio(region)
  dark = region(:, :, 1) < 180 & region(:, :, 2) < 180 & region(:, :, 3) < 180;
  ratio = sum(dark(:)) / numel(dark);
end
%=============================================================================
function len = bottom_tick_length(frame, ax, direction)
  [left, ~, right, bottom] = axes_edges(frame, ax);
  x = round((left + right) / 2);
  len = 0;
  started = false;
  if strcmp(direction, 'in')
    rows = (bottom - 1):-1:1;
  else
    rows = (bottom + 1):size(frame, 1);
  end
  for y = rows
    region = frame(y, x - 1:x + 1, :);
    hit = dark_ratio(region) > 0;
    if hit
      len = len + 1;
      started = true;
    elseif started
      return;
    end
  end
end
%=============================================================================
function count = tick_label_dark_pixel_count(frame, ax)
  [left, top, ~, bottom] = axes_edges(frame, ax);
  dark = frame(:, :, 1) < 180 & frame(:, :, 2) < 180 & frame(:, :, 3) < 180;
  dark(max(1, bottom - 2):min(size(frame, 1), bottom + 2), :) = false;
  dark(:, max(1, left - 2):min(size(frame, 2), left + 2)) = false;
  labelMask = false(size(dark));
  labelMask(max(1, top - 30):min(size(frame, 1), bottom + 30), :) = true;
  count = sum(dark(:) & labelMask(:));
end
%=============================================================================
