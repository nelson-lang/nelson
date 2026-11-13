%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Simulate a Gray-Scott reaction-diffusion system.
rng(42);
gridSize = 100;
u = ones(gridSize);
v = zeros(gridSize);
seedRange = 43:58;
u(seedRange, seedRange) = 0.50 + 0.04 * rand(length(seedRange));
v(seedRange, seedRange) = 0.25 + 0.04 * rand(length(seedRange));
% Coral-growth regime of the classic 9-point scheme (weights sum to zero).
diffusionU = 1.0;
diffusionV = 0.5;
feedRate = 0.055;
killRate = 0.062;
timeStep = 1;

figureHandle = figure('SelectionHighlight', 'off', 'ToolBar', 'none', 'Name', 'Gray-Scott reaction diffusion', ...
  'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(axesHandle);
imageHandle = image('Parent', axesHandle, 'CData', v, ...
  'CDataMapping', 'scaled');
axis(axesHandle, 'image');
axis(axesHandle, 'off');
colormap(axesHandle, parula(256));

stepsPerFrame = 20;
for frameIndex = 1:150
  if ~isgraphics(figureHandle) || ~isgraphics(imageHandle)
    break
  end
  for simulationStep = 1:stepsPerFrame
    laplacianU = grayScottLaplacian(u);
    laplacianV = grayScottLaplacian(v);
    reaction = u .* v .^ 2;
    u = u + (diffusionU * laplacianU - reaction + feedRate * (1 - u)) * timeStep;
    v = v + (diffusionV * laplacianV + reaction - ...
      (feedRate + killRate) * v) * timeStep;
    u = min(max(u, 0), 1);
    v = min(max(v, 0), 1);
  end
  set(imageHandle, 'CData', v);
  title(axesHandle, sprintf('Gray-Scott model - step %d', frameIndex * stepsPerFrame));
  drawnow();
end
%=============================================================================
function L = grayScottLaplacian(X)
  % 9-point stencil on a periodic grid: 0.2 per edge neighbour, 0.05 per corner.
  L = 0.2 * (circshift(X, 1, 1) + circshift(X, -1, 1) + circshift(X, 1, 2) + circshift(X, -1, 2)) ...
    + 0.05 * (circshift(X, [1 1]) + circshift(X, [1 -1]) + circshift(X, [-1 1]) + circshift(X, [-1 -1])) ...
    - X;
end
%=============================================================================
