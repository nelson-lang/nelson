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
patch = 43:58;
u(patch, patch) = 0.50 + 0.04 * rand(length(patch));
v(patch, patch) = 0.25 + 0.04 * rand(length(patch));
diffusionU = 0.16;
diffusionV = 0.08;
feedRate = 0.060;
killRate = 0.062;
timeStep = 1;
laplacianKernel = [0 0.20 0; 0.20 -1 0.20; 0 0.20 0];

figureHandle = figure('Name', 'Gray-Scott reaction diffusion', ...
  'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
imageHandle = image('Parent', axesHandle, 'CData', v, ...
  'CDataMapping', 'scaled');
axis(axesHandle, 'image');
axis(axesHandle, 'off');
colormap(axesHandle, parula(256));

for frameIndex = 1:65
  if ~isgraphics(figureHandle) || ~isgraphics(imageHandle)
    break
  end
  for simulationStep = 1:8
    laplacianU = conv2(u, laplacianKernel, 'same');
    laplacianV = conv2(v, laplacianKernel, 'same');
    reaction = u .* v .^ 2;
    u = u + (diffusionU * laplacianU - reaction + feedRate * (1 - u)) * timeStep;
    v = v + (diffusionV * laplacianV + reaction - ...
      (feedRate + killRate) * v) * timeStep;
    u = min(max(u, 0), 1);
    v = min(max(v, 0), 1);
  end
  set(imageHandle, 'CData', v);
  title(axesHandle, sprintf('Gray-Scott model - step %d', frameIndex * 8));
  drawnow();
end
%=============================================================================
