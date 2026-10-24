%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Detect edges in a self-contained synthetic color image.
imageSize = 256;
[x, y] = meshgrid(linspace(-1, 1, imageSize));
red = 0.15 + 0.75 * exp(-7 * ((x + 0.35) .^ 2 + (y + 0.10) .^ 2));
green = 0.10 + 0.80 * exp(-10 * ((x - 0.35) .^ 2 + (y - 0.20) .^ 2));
blue = 0.20 + 0.60 * (sin(5 * x) .* cos(4 * y) + 1) / 2;
red(abs(x) < 0.30 & abs(y) < 0.16) = 0.95;
colorImage = min(max(cat(3, red, green, blue), 0), 1);
grayImage = 0.2989 * colorImage(:, :, 1) + ...
  0.5870 * colorImage(:, :, 2) + 0.1140 * colorImage(:, :, 3);

verticalKernel = [-1 0 1; -2 0 2; -1 0 1];
horizontalKernel = verticalKernel.';
verticalEdges = conv2(grayImage, verticalKernel, 'same');
horizontalEdges = conv2(grayImage, horizontalKernel, 'same');
edgeMagnitude = sqrt(verticalEdges .^ 2 + horizontalEdges .^ 2);
edgeMagnitude = edgeMagnitude / max(edgeMagnitude(:));

figureHandle = figure('Name', 'Sobel edge detection', 'NumberTitle', 'off');
colorAxes = subplot(1, 3, 1, 'Parent', figureHandle);
image('Parent', colorAxes, 'CData', colorImage);
axis(colorAxes, 'image');
axis(colorAxes, 'off');
title(colorAxes, 'Synthetic color image');
grayAxes = subplot(1, 3, 2, 'Parent', figureHandle);
image('Parent', grayAxes, 'CData', grayImage, 'CDataMapping', 'scaled');
axis(grayAxes, 'image');
axis(grayAxes, 'off');
title(grayAxes, 'Grayscale');
edgeAxes = subplot(1, 3, 3, 'Parent', figureHandle);
image('Parent', edgeAxes, 'CData', edgeMagnitude, 'CDataMapping', 'scaled');
axis(edgeAxes, 'image');
axis(edgeAxes, 'off');
colormap(grayAxes, gray(256));
colormap(edgeAxes, gray(256));
title(edgeAxes, 'Sobel magnitude');
%=============================================================================
