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
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
ax2 = axes('Parent', f);
asserts.isequal(f.CurrentAxes, ax2);
f.CurrentAxes = ax1;
asserts.isequal(gca(), ax1);
l = line('Parent', ax1, 'XData', 1:3, 'YData', 1:3);
f.CurrentObject = l;
asserts.isequal(gco(), l);
f.Colormap = gray(8);
asserts.isequal(ax1.Colormap, gray(8));
asserts.isequal(ax2.Colormap, gray(8));
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
