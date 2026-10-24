%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Grow a binary fractal tree without recursive function calls.
generationCount = 11;
branchScale = 0.72;
branchAngle = pi / 6;
segmentStart = [0 0];
segmentEnd = [0 1];
plotX = [0; 0; NaN];
plotY = [0; 1; NaN];

for generation = 1:generationCount
  direction = (segmentEnd - segmentStart) * branchScale;
  leftDirection = [...
    cos(branchAngle) * direction(:, 1) - sin(branchAngle) * direction(:, 2), ...
    sin(branchAngle) * direction(:, 1) + cos(branchAngle) * direction(:, 2)];
  rightDirection = [...
    cos(branchAngle) * direction(:, 1) + sin(branchAngle) * direction(:, 2), ...
    -sin(branchAngle) * direction(:, 1) + cos(branchAngle) * direction(:, 2)];
  nextStart = [segmentEnd; segmentEnd];
  nextEnd = [segmentEnd + leftDirection; segmentEnd + rightDirection];
  xSegments = [nextStart(:, 1), nextEnd(:, 1), NaN(size(nextStart, 1), 1)].';
  ySegments = [nextStart(:, 2), nextEnd(:, 2), NaN(size(nextStart, 1), 1)].';
  plotX = [plotX; xSegments(:)];
  plotY = [plotY; ySegments(:)];
  segmentStart = nextStart;
  segmentEnd = nextEnd;
end

figureHandle = figure('Name', 'Fractal tree', 'NumberTitle', 'off', ...
  'Color', [0.04 0.06 0.08]);
axesHandle = axes('Parent', figureHandle, 'Color', [0.04 0.06 0.08]);
plot(axesHandle, plotX, plotY, 'Color', [0.35 0.85 0.45], 'LineWidth', 0.7);
axis(axesHandle, 'equal');
axis(axesHandle, 'off');
title(axesHandle, sprintf('Binary fractal tree, %d generations', generationCount), ...
  'Color', [0.85 0.95 0.85]);
%=============================================================================
