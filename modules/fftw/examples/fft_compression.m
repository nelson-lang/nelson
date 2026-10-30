%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Keep a growing fraction of the fft2 coefficients and rebuild the image live.
if ~exist('fftCompFigureVisible', 'var')
  fftCompFigureVisible = 'on';
end
if ~exist('fftCompAnimationEnabled', 'var')
  fftCompAnimationEnabled = true;
end
if ~exist('fftCompAnimationFrames', 'var')
  fftCompAnimationFrames = 260;
end
if ~exist('fftCompFramePause', 'var')
  fftCompFramePause = 0;
end
if ~exist('fftCompAnimationContinuous', 'var')
  fftCompAnimationContinuous = strcmp(fftCompFigureVisible, 'on');
end
if ~exist('fftCompSize', 'var')
  fftCompSize = 256;
end
if ~exist('fftCompStartFraction', 'var')
  fftCompStartFraction = 0.0004;
end
if ~exist('fftCompEndFraction', 'var')
  fftCompEndFraction = 1.0;
end

fftCompImage = fftCompSyntheticImage(fftCompSize);
fftCompSpectrum = fft2(fftCompImage);
fftCompMagnitudes = abs(fftCompSpectrum);
fftCompSortedMagnitudes = sort(fftCompMagnitudes(:), 'descend');
fftCompTotal = numel(fftCompSpectrum);
fftCompFrameCount = max(2, fftCompAnimationFrames);
% Sweep the kept fraction on a logarithmic scale from very sparse to full.
fftCompFractions = logspace(log10(fftCompStartFraction), ...
  log10(fftCompEndFraction), fftCompFrameCount);

fftCompFigure = figure('Name', 'FFT image compression', ...
  'NumberTitle', 'off', 'Color', [0.05 0.06 0.08], ...
  'Visible', fftCompFigureVisible, 'Position', [80 140 1120 560]);
fftCompSourceAxes = axes('Parent', fftCompFigure, ...
  'Position', [0.03 0.08 0.44 0.82]);
image(fftCompSourceAxes, 'CData', fftCompToRgb(fftCompImage));
axis(fftCompSourceAxes, 'image');
axis(fftCompSourceAxes, 'off');
title(fftCompSourceAxes, 'Original', 'Color', [0.92 0.94 0.98], ...
  'FontSize', 13);
fftCompReconAxes = axes('Parent', fftCompFigure, ...
  'Position', [0.52 0.08 0.44 0.82]);
% Prevent accidental pan or zoom from disturbing the live reconstruction.
disableDefaultInteractivity(fftCompReconAxes);
fftCompReconImage = image(fftCompReconAxes, 'CData', ...
  fftCompToRgb(zeros(fftCompSize, fftCompSize)));
axis(fftCompReconAxes, 'image');
axis(fftCompReconAxes, 'off');
fftCompReconTitle = title(fftCompReconAxes, '', 'Color', [0.92 0.94 0.98], ...
  'FontSize', 13);
fftCompInfoText = uicontrol('Parent', fftCompFigure, 'Style', 'text', ...
  'String', '', 'Position', [20 8 900 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.08], 'ForegroundColor', [0.78 0.86 0.96], ...
  'FontSize', 10);

if fftCompAnimationEnabled
  while isgraphics(fftCompFigure)
    for fftCompFrameIndex = 1:fftCompFrameCount
      if ~isgraphics(fftCompFigure)
        break
      end
      try
        fraction = fftCompFractions(fftCompFrameIndex);
        keep = max(1, round(fraction * fftCompTotal));
        threshold = fftCompSortedMagnitudes(keep);
        % Zero every coefficient weaker than the threshold, then invert.
        mask = fftCompMagnitudes >= threshold;
        reconstruction = real(ifft2(fftCompSpectrum .* mask));
        set(fftCompReconImage, 'CData', fftCompToRgb(reconstruction));
        keptCount = sum(mask(:));
        errorEnergy = mean((reconstruction(:) - fftCompImage(:)) .^ 2);
        psnr = 10 * log10(1 / max(errorEnergy, 1e-12));
        set(fftCompReconTitle, 'String', sprintf(...
          'Reconstructed from %.2f%% of coefficients', ...
          100 * keptCount / fftCompTotal));
        if isgraphics(fftCompInfoText)
          set(fftCompInfoText, 'String', sprintf(...
            '%d of %d fft2 coefficients kept | PSNR %.1f dB', ...
            keptCount, fftCompTotal, psnr));
        end
        drawnow();
      catch
        % The figure was closed mid-frame; stop the animation cleanly.
        break
      end
      if fftCompFramePause > 0
        pause(fftCompFramePause);
      end
    end
    if ~fftCompAnimationContinuous || ~isgraphics(fftCompFigure)
      break
    end
  end
end
%=============================================================================
function image = fftCompSyntheticImage(side)
  % A high-contrast scene with edges and stripes so lost detail is visible.
  axis1d = linspace(-1, 1, side);
  [x, y] = meshgrid(axis1d, axis1d);
  image = zeros(side, side);
  image = image + 0.8 * (x .^ 2 + y .^ 2 < 0.55 ^ 2);
  image = image + 0.5 * (abs(x) < 0.12);
  image = image + 0.4 * (y > 0.62);
  image = image + 0.25 * (sin(26 * x) > 0.6);
  image = image - 0.6 * ((x - 0.45) .^ 2 + (y + 0.45) .^ 2 < 0.18 ^ 2);
  image = image - min(image(:));
  image = image / max(image(:));
end
%=============================================================================
function rgb = fftCompToRgb(gray)
  clipped = min(max(gray, 0), 1);
  rgb = zeros(size(gray, 1), size(gray, 2), 3);
  rgb(:, :, 1) = clipped;
  rgb(:, :, 2) = clipped;
  rgb(:, :, 3) = clipped;
end
%=============================================================================
