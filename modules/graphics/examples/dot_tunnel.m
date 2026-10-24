%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate rings of points moving through a tunnel.
ringCount = 45;
dotsPerRing = 45;
outerRadius = 120;
innerRadius = 1.5;
shrinkFactor = 0.94;
rotationPerFrame = 0.015;
framesPerSecond = 30;
frameCount = 90;

radialDistribution = linspace(0, 1, ringCount) .^ 1.4;
initialRadii = innerRadius + ...
  (outerRadius - innerRadius) * radialDistribution;
angles = linspace(0, 2 * pi, dotsPerRing + 1);
angles(end) = [];
[angleGrid, radiusGrid] = meshgrid(angles, initialRadii);

x = radiusGrid .* cos(angleGrid);
y = radiusGrid .* sin(angleGrid);
brightness = 1 - radiusGrid(:) / outerRadius;
brightness(brightness < 0.15) = 0.15;
colors = [brightness brightness brightness];

figureHandle = figure('Color', 'k', 'Name', 'Dot tunnel', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle, 'Color', 'k');
scatterHandle = scatter(axesHandle, x(:), y(:), 18, colors, 'filled');
axis(axesHandle, 'equal');
axis(axesHandle, 'off');
axis(axesHandle, [-outerRadius outerRadius -outerRadius outerRadius]);
set(axesHandle, 'ZLim', [-1 1], 'ZTick', [-1 0 1]);

frameDuration = 1 / framesPerSecond;
for frameIndex = 1:frameCount
  if ~isgraphics(figureHandle) || ~isgraphics(scatterHandle)
    break
  end
  frameTimer = tic();
  angleGrid = angleGrid + rotationPerFrame;
  radiusGrid = radiusGrid * shrinkFactor;
  radiusGrid(radiusGrid < innerRadius) = outerRadius;
  x = radiusGrid .* cos(angleGrid);
  y = radiusGrid .* sin(angleGrid);
  brightness = 1 - radiusGrid(:) / outerRadius;
  brightness(brightness < 0.15) = 0.15;
  colors = [brightness brightness brightness];
  set(scatterHandle, 'XData', x(:), 'YData', y(:), 'CData', colors);
  drawnow();
  pause(max(0, frameDuration - toc(frameTimer)));
end
%=============================================================================
