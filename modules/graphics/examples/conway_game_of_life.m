%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Animate Conway's Game of Life from a reproducible random population.
rng(42);
gridSize = 90;
frameCount = 75;
population = rand(gridSize) < 0.28;
neighborKernel = ones(3);

figureHandle = figure('Name', 'Conway Game of Life', 'NumberTitle', 'off');
axesHandle = axes('Parent', figureHandle);
imageHandle = image('Parent', axesHandle, 'CData', double(population), ...
  'CDataMapping', 'scaled');
axis(axesHandle, 'image');
axis(axesHandle, 'off');
colormap(axesHandle, [0.04 0.05 0.08; 0.25 0.90 0.55]);

for frameIndex = 1:frameCount
  if ~isgraphics(figureHandle) || ~isgraphics(imageHandle)
    break
  end
  neighborCount = conv2(double(population), neighborKernel, 'same') - population;
  population = neighborCount == 3 | (population & neighborCount == 2);
  set(imageHandle, 'CData', double(population));
  title(axesHandle, sprintf('Conway Game of Life - generation %d', frameIndex));
  drawnow();
  pause(0.025);
end
%=============================================================================
