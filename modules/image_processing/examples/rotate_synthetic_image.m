%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Rotate a synthetic color image with bilinear interpolation.
imageSize = 160;
[x, y] = meshgrid(linspace(-1, 1, imageSize));
checkerboard = mod(floor((x + 1) * 8) + floor((y + 1) * 8), 2);
disc = x .^ 2 + y .^ 2 <= 0.72 ^ 2;
red = 0.15 + 0.75 * checkerboard;
green = 0.20 + 0.70 * (1 - checkerboard);
blue = 0.25 + 0.65 * disc;
imageData = uint8(255 * cat(3, red .* disc, green .* disc, blue));

figureHandle = figure('Name', 'Bilinear image rotation', 'NumberTitle', 'off');
imageHandle = imshow(imageData);
title('Rotating a synthetic image');

framesPerSecond = 24;
frameCount = 72;
frameDuration = 1 / framesPerSecond;
for frameIndex = 1:frameCount
  if ~isgraphics(figureHandle) || ~isgraphics(imageHandle)
    break
  end
  frameTimer = tic();
  angle = 360 * (frameIndex - 1) / frameCount;
  rotatedImage = imrotate(imageData, angle, 'bilinear', 'crop');
  set(imageHandle, 'CData', rotatedImage);
  drawnow();
  pause(max(0, frameDuration - toc(frameTimer)));
end
%=============================================================================
