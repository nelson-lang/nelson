%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Show the SHA-256 avalanche: a tiny input change flips about half the bits.
if ~exist('avalancheFigureVisible', 'var')
  avalancheFigureVisible = 'on';
end
if ~exist('avalancheAnimationEnabled', 'var')
  avalancheAnimationEnabled = true;
end
if ~exist('avalancheAnimationFrames', 'var')
  avalancheAnimationFrames = 600;
end
if ~exist('avalancheFramePause', 'var')
  avalancheFramePause = 0.03;
end
if ~exist('avalancheAnimationContinuous', 'var')
  avalancheAnimationContinuous = strcmp(avalancheFigureVisible, 'on');
end
if ~exist('avalancheMessage', 'var')
  avalancheMessage = 'Nelson block ';
end

avalanchePrevBits = zeros(1, 256);
avalancheImageData = avalancheColorize(reshape(avalanchePrevBits, 16, 16), ...
  false(16, 16));

avalancheFigure = figure('Name', 'SHA-256 avalanche', 'NumberTitle', 'off', ...
  'Color', [0.04 0.05 0.08], 'Visible', avalancheFigureVisible, ...
  'Position', [140 110 680 720]);
avalancheAxes = axes('Parent', avalancheFigure, ...
  'Position', [0.05 0.08 0.90 0.82]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(avalancheAxes);
avalancheImage = image(avalancheAxes, 'CData', avalancheImageData);
axis(avalancheAxes, 'image');
axis(avalancheAxes, 'off');
avalancheTitle = title(avalancheAxes, 'SHA-256 of the input, drawn as 256 bits', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
avalancheInfo = uicontrol('Parent', avalancheFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 640 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.04 0.05 0.08], 'ForegroundColor', [0.78 0.86 0.96], ...
  'FontSize', 10);

if avalancheAnimationEnabled
  avalancheFrame = 0;
  while isgraphics(avalancheFigure)
    avalancheFrame = avalancheFrame + 1;
    if avalancheFrame > avalancheAnimationFrames && ...
        ~avalancheAnimationContinuous
      break
    end
    if ~isgraphics(avalancheFigure)
      break
    end
    try
      % Append a counter so each frame hashes an input differing by little.
      input = [avalancheMessage num2str(avalancheFrame)];
      digest = lower(sha256(input));
      bits = avalancheHexToBits(digest);
      changed = bits ~= avalanchePrevBits;
      flipped = sum(changed);
      set(avalancheImage, 'CData', avalancheColorize(...
        reshape(bits, 16, 16), reshape(changed, 16, 16)));
      set(avalancheTitle, 'String', sprintf(...
        'SHA-256("%s") as 256 bits', input));
      if isgraphics(avalancheInfo)
        set(avalancheInfo, 'String', sprintf(...
          'bits changed vs previous hash: %d / 256  (%.0f%% avalanche)', ...
          flipped, 100 * flipped / 256));
      end
      avalanchePrevBits = bits;
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if avalancheFramePause > 0
      pause(avalancheFramePause);
    end
  end
end
%=============================================================================
function bits = avalancheHexToBits(digest)
  % Turn the 64 hex characters of the digest into a 256-bit vector.
  hexAlphabet = '0123456789abcdef';
  bits = zeros(1, 256);
  for charIndex = 1:numel(digest)
    value = find(hexAlphabet == digest(charIndex), 1) - 1;
    if isempty(value)
      value = 0;
    end
    bits((charIndex - 1) * 4 + (1:4)) = mod(floor(value ./ [8 4 2 1]), 2);
  end
end
%=============================================================================
function rgb = avalancheColorize(bitGrid, changedGrid)
  % Set bits glow cyan; bits that just flipped are highlighted in orange.
  rows = size(bitGrid, 1);
  cols = size(bitGrid, 2);
  rgb = zeros(rows, cols, 3);
  setBit = bitGrid ~= 0;
  changedSet = changedGrid & setBit;
  steadySet = setBit & ~changedGrid;
  red = 0.10 * ones(rows, cols);
  green = 0.12 * ones(rows, cols);
  blue = 0.18 * ones(rows, cols);
  red(steadySet) = 0.30;
  green(steadySet) = 0.85;
  blue(steadySet) = 0.95;
  red(changedSet) = 0.98;
  green(changedSet) = 0.62;
  blue(changedSet) = 0.20;
  rgb(:, :, 1) = red;
  rgb(:, :, 2) = green;
  rgb(:, :, 3) = blue;
end
%=============================================================================
