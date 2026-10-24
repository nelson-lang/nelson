%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build a colorful mathematical image from vectorized waves and rings.
imageWidth = 480;
imageHeight = 360;
[x, y] = meshgrid(linspace(-1.2, 1.2, imageWidth), ...
  linspace(0.9, -0.9, imageHeight));
transformedX = x - y;
transformedY = 2 * x + 2 * y;
denominator = 2.4 - transformedY;
projectedX = transformedX ./ denominator;
projectedY = transformedY ./ denominator;
radius = sqrt(projectedX .^ 2 + projectedY .^ 2);
angle = atan2(projectedY, projectedX);
colorImage = zeros(imageHeight, imageWidth, 3);
phaseTime = 0.75;

for ringIndex = 1:30
  phase = angle * ceil(0.1 * ringIndex) + ...
    phaseTime * sin(ringIndex ^ 2) + ringIndex ^ 2;
  glow = 0.18 ./ (abs(radius * 70 - ringIndex) + 0.055);
  wave = min(max(cos(phase), 0), 0.6);
  for channelIndex = 1:3
    colorWave = cos(phase - ringIndex + channelIndex - 1) + 1;
    colorImage(:, :, channelIndex) = colorImage(:, :, channelIndex) + ...
      glow .* wave .* colorWave;
  end
end
colorImage = colorImage / max(colorImage(:));
colorImage = min(sqrt(colorImage), 1);

figureHandle = figure('Name', 'Mathematical shader', 'NumberTitle', 'off', ...
  'Color', 'black');
axesHandle = axes('Parent', figureHandle);
image('Parent', axesHandle, 'CData', colorImage);
axis(axesHandle, 'image');
axis(axesHandle, 'off');
title(axesHandle, 'Vectorized mathematical shader', 'Color', 'white');
%=============================================================================
