%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Remove salt-and-pepper noise from a synthetic grayscale image with a median filter.
rng(7);
[gridX, gridY] = meshgrid(linspace(-1, 1, 200), linspace(-1, 1, 160));
cleanImage = 0.25 + 0.25 * (gridX + 1);
cleanImage(gridX .^ 2 + gridY .^ 2 < 0.35) = 0.9;
cleanImage(abs(gridX) < 0.08 & abs(gridY) < 0.7) = 0.1;
noiseDraw = rand(size(cleanImage));
noisyImage = cleanImage;
noisyImage(noiseDraw < 0.04) = 0;
noisyImage(noiseDraw > 0.96) = 1;
filtered = medfilt2(noisyImage, [3, 3], 'symmetric');
psnrOf = @(img) 10 * log10(1 / mean((img(:) - cleanImage(:)) .^ 2));
figure('Name', 'Median filter denoising', 'NumberTitle', 'off');
subplot(1, 3, 1); imshow(cleanImage); title('Original');
subplot(1, 3, 2); imshow(noisyImage); title(sprintf('8%% salt & pepper (PSNR %.1f dB)', psnrOf(noisyImage)));
subplot(1, 3, 3); imshow(filtered); title(sprintf('Median 3x3 (PSNR %.1f dB)', psnrOf(filtered)));
