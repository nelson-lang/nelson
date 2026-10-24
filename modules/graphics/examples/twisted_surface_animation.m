%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate a bounded sequence of twisted parametric surfaces.
radialSamples = 20;
angularSamples = 180;
frameCount = 75;
angle = linspace(0, 2 * pi, angularSamples);
radialPosition = linspace(-1, 1, radialSamples).';
cosineGrid = repmat(cos(angle), radialSamples, 1);
sineGrid = repmat(sin(angle), radialSamples, 1);
twist = zeros(1, angularSamples);
radialCosine = radialPosition * cos(twist);
radialSine = radialPosition * sin(twist);

figureHandle = figure('SelectionHighlight', 'off', 'Name', 'Twisted surface animation', ...
  'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
surfaceHandle = surf(axesHandle, (1 + radialCosine) .* cosineGrid, ...
  (1 + radialCosine) .* sineGrid, radialSine, radialSine);
set(surfaceHandle, 'EdgeColor', 'none');
axis(axesHandle, 'equal');
axis(axesHandle, [-2 2 -2 2 -1.2 1.2]);
set(axesHandle, 'XTick', -2:1:2, 'YTick', -2:0.5:2);
axis(axesHandle, 'off');
view(axesHandle, 3);
colormap(axesHandle, parula(256));

for frameIndex = 1:frameCount
  if ~isgraphics(figureHandle) || ~isgraphics(surfaceHandle)
    break
  end
  phase = 2 * pi * (frameIndex - 1) / frameCount;
  twist = 3 * pi * sin(phase) * sin(angle + phase);
  radialCosine = radialPosition * cos(twist);
  radialSine = radialPosition * sin(twist);
  set(surfaceHandle, 'XData', (1 + radialCosine) .* cosineGrid, ...
    'YData', (1 + radialCosine) .* sineGrid, 'ZData', radialSine, ...
    'CData', radialSine);
  title(axesHandle, sprintf('Twisted surface - frame %d', frameIndex));
  drawnow();
  pause(0.015);
end
%=============================================================================
