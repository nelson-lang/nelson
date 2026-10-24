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
ax = axes('Parent', f);
x = [-102.78651428222656, -7.8202972412109375];
y = [-11.798049926757812, 161.43443298339844];
z = [-17.131217956542969, 30.384407043457031];
plot3(ax, x, y, z);
axes(ax);
axis equal;
view(3);
drawnow();
limits = [xlim(), ylim(), zlim()];
asserts.isapprox(limits, [x, -20, 180, z], 1e-12);
asserts.isequal(get(ax, 'XTick'), -100:20:-20);
asserts.isequal(get(ax, 'YTick'), [0 50 100 150]);
asserts.isequal(get(ax, 'ZTick'), -10:10:30);
center = [(limits(1) + limits(2)) / 2, (limits(3) + limits(4)) / 2, ...
  (limits(5) + limits(6)) / 2];
radius = 10 * norm([(limits(2) - limits(1)) / 2, (limits(4) - limits(3)) / 2, ...
  (limits(6) - limits(5)) / 2]);
azimuth = -37.5 * pi / 180;
elevation = 30 * pi / 180;
expectedCameraPosition = [cos(elevation) * sin(azimuth) * radius + center(1), ...
  -cos(elevation) * cos(azimuth) * radius + center(2), ...
  sin(elevation) * radius + center(3)];
asserts.isapprox(ax.CameraPosition, expectedCameraPosition, 1e-10);
forward = center - expectedCameraPosition;
distance = norm(forward);
forward = forward ./ distance;
right = cross(forward, [0, 0, 1]);
right = right ./ norm(right);
cameraUp = cross(right, forward);
cameraUp = cameraUp ./ norm(cameraUp);
corners = [limits(1) limits(3) limits(5); limits(1) limits(3) limits(6); ...
  limits(1) limits(4) limits(5); limits(1) limits(4) limits(6); ...
  limits(2) limits(3) limits(5); limits(2) limits(3) limits(6); ...
  limits(2) limits(4) limits(5); limits(2) limits(4) limits(6)];
relative = corners - expectedCameraPosition;
projectedX = relative * right';
projectedY = relative * cameraUp';
figPosition = f.Position;
axesPosition = ax.Position;
axesWidth = axesPosition(3) * figPosition(3);
axesHeight = axesPosition(4) * figPosition(4);
xSpan = max(projectedX) - min(projectedX);
ySpan = max(projectedY) - min(projectedY);
% The angle spans the extent that fills the axes: the width when the box is
% relatively wider than the axes, the height otherwise.
if (xSpan * axesHeight >= ySpan * axesWidth)
  span = xSpan;
else
  span = ySpan;
end
expectedCameraViewAngle = 360 / pi * atan(span / (2 * distance));
asserts.isapprox(ax.CameraViewAngle, expectedCameraViewAngle, 1e-10);
close(f);
close all
%=============================================================================
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
