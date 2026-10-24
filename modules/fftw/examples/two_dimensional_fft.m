%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Display the centered spectrum of a simple two-dimensional pattern.
imageSize = 128;
spatialPattern = zeros(imageSize);
spatialPattern(25:104, 57:72) = 1;
spatialPattern(57:72, 25:104) = 1;
centeredTransform = fftshift(fft2(spatialPattern));
logMagnitude = log(1 + abs(centeredTransform));

if ~exist('fftExampleVisible', 'var')
  fftExampleVisible = 'on';
end
fftExampleFigure = figure('Name', 'Two-dimensional Fourier transform', ...
  'NumberTitle', 'off', 'Visible', fftExampleVisible);
subplot(1, 2, 1);
imagesc(spatialPattern);
axis('image');
title('Spatial pattern');
subplot(1, 2, 2);
imagesc(logMagnitude);
axis('image');
title('Centered log magnitude');
colormap(parula(128));
