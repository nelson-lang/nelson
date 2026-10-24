%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Capture a rotating parametric surface as an animated GIF.
surfaceSize = 96;
longitude = linspace(0, 2 * pi, surfaceSize);
latitude = linspace(0, pi, surfaceSize);
[longitudeGrid, latitudeGrid] = meshgrid(longitude, latitude);

x = sin(latitudeGrid) .* cos(longitudeGrid);
y = 1.25 * cos(latitudeGrid) + 0.08 * cos(2 * latitudeGrid);
z = 0.90 * sin(latitudeGrid) .* sin(longitudeGrid);
pattern = sin(5 * latitudeGrid) .* cos(3 * longitudeGrid) + ...
  0.5 * sin(8 * longitudeGrid) + 0.3 * cos(10 * latitudeGrid);

figureHandle = figure('Color', 'w', 'Name', 'Animated surface GIF', ...
  'NumberTitle', 'off', 'Position', [100 100 480 480]);
axesHandle = axes('Parent', figureHandle);
surfaceHandle = surf(axesHandle, x, y, z, pattern, ...
  'EdgeColor', 'none', 'FaceColor', 'interp');
axis(axesHandle, 'equal');
axis(axesHandle, 'off');
colormap(axesHandle, parula(128));
title(axesHandle, 'Rotating parametric surface', 'Visible', 'on');

gifFilename = [tempname(), '.gif'];
frameCount = 24;
delayTime = 0.05;
viewAngles = linspace(0, 360, frameCount + 1);
viewAngles(end) = [];

for frameIndex = 1:frameCount
  if ~isgraphics(figureHandle) || ~isgraphics(surfaceHandle)
    break
  end
  view(axesHandle, viewAngles(frameIndex), 20);
  drawnow();
  frameData = getframe(figureHandle).cdata;
  if frameIndex == 1
    imwrite(frameData, gifFilename, 'gif', ...
      'LoopCount', Inf, 'DelayTime', delayTime);
  else
    imwrite(frameData, gifFilename, 'gif', ...
      'WriteMode', 'append', 'DelayTime', delayTime);
  end
end

if exist(gifFilename, 'file')
  fprintf('Animated GIF written to: %s\n', gifFilename);
end
%=============================================================================
