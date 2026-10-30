%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Grow Pascal's triangle and colour its odd entries into a Sierpinski gasket.
if ~exist('sierpinskiFigureVisible', 'var')
  sierpinskiFigureVisible = 'on';
end
if ~exist('sierpinskiAnimationEnabled', 'var')
  sierpinskiAnimationEnabled = true;
end
if ~exist('sierpinskiAnimationFrames', 'var')
  sierpinskiAnimationFrames = 520;
end
if ~exist('sierpinskiFramePause', 'var')
  sierpinskiFramePause = 0;
end
if ~exist('sierpinskiAnimationContinuous', 'var')
  sierpinskiAnimationContinuous = strcmp(sierpinskiFigureVisible, 'on');
end
% Largest triangle order; capped so the binomial coefficients stay exact
% integers before the parity is taken.
if ~exist('sierpinskiMaxOrder', 'var')
  sierpinskiMaxOrder = 54;
end

sierpinskiOddColor = [0.98 0.80 0.25];
sierpinskiEvenColor = [0.06 0.07 0.12];
sierpinskiOrder = 2;
sierpinskiImageData = sierpinskiColorize(sierpinskiTriangle(sierpinskiOrder), ...
  sierpinskiOddColor, sierpinskiEvenColor);

sierpinskiFigure = figure('Name', 'Pascal to Sierpinski', ...
  'NumberTitle', 'off', 'Color', [0.06 0.07 0.12], ...
  'Visible', sierpinskiFigureVisible, 'Position', [130 80 820 820]);
sierpinskiAxes = axes('Parent', sierpinskiFigure, ...
  'Position', [0.05 0.08 0.90 0.84], 'Color', [0.06 0.07 0.12]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(sierpinskiAxes);
sierpinskiImage = image(sierpinskiAxes, 'XData', [1 sierpinskiOrder], ...
  'YData', [1 sierpinskiOrder], 'CData', sierpinskiImageData);
axis(sierpinskiAxes, 'image');
axis(sierpinskiAxes, 'off');
sierpinskiTitle = title(sierpinskiAxes, '', 'Color', [0.92 0.94 0.98], ...
  'FontSize', 13);
sierpinskiInfoText = uicontrol('Parent', sierpinskiFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 10 780 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.06 0.07 0.12], 'ForegroundColor', [0.85 0.8 0.6], ...
  'FontSize', 10);

if sierpinskiAnimationEnabled
  sierpinskiFrame = 0;
  while isgraphics(sierpinskiFigure)
    sierpinskiFrame = sierpinskiFrame + 1;
    if sierpinskiFrame > sierpinskiAnimationFrames && ...
        ~sierpinskiAnimationContinuous
      break
    end
    if ~isgraphics(sierpinskiFigure)
      break
    end
    try
      % Sweep the order up to the cap, hold briefly, then restart.
      span = sierpinskiMaxOrder - 2;
      phase = mod(sierpinskiFrame - 1, span + 12);
      sierpinskiOrder = 2 + min(phase, span);
      triangle = sierpinskiTriangle(sierpinskiOrder);
      set(sierpinskiImage, 'XData', [1 sierpinskiOrder], ...
        'YData', [1 sierpinskiOrder], 'CData', sierpinskiColorize(...
        triangle, sierpinskiOddColor, sierpinskiEvenColor));
      set(sierpinskiAxes, 'XLim', [0.5 sierpinskiOrder + 0.5], ...
        'YLim', [0.5 sierpinskiOrder + 0.5]);
      set(sierpinskiTitle, 'String', sprintf(...
        'Pascal''s triangle mod 2, order %d', sierpinskiOrder));
      if isgraphics(sierpinskiInfoText)
        set(sierpinskiInfoText, 'String', sprintf(...
          'odd binomial coefficients C(i,j) lit | %d rows', sierpinskiOrder));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if sierpinskiFramePause > 0
      pause(sierpinskiFramePause);
    end
  end
end
%=============================================================================
function triangle = sierpinskiTriangle(order)
  % Parity of the lower-triangular Pascal matrix: odd entries form Sierpinski.
  triangle = mod(pascal(order, 1), 2);
end
%=============================================================================
function rgb = sierpinskiColorize(triangle, oddColor, evenColor)
  odd = double(triangle ~= 0);
  even = 1 - odd;
  rgb = zeros(size(triangle, 1), size(triangle, 2), 3);
  for channel = 1:3
    rgb(:, :, channel) = odd * oddColor(channel) + even * evenColor(channel);
  end
end
%=============================================================================
