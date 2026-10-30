%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Draw times-table chords on a circle: point i joins point (k*i mod n).
if ~exist('modMultFigureVisible', 'var')
  modMultFigureVisible = 'on';
end
if ~exist('modMultAnimationEnabled', 'var')
  modMultAnimationEnabled = true;
end
if ~exist('modMultAnimationFrames', 'var')
  modMultAnimationFrames = 420;
end
if ~exist('modMultFramePause', 'var')
  modMultFramePause = 0;
end
if ~exist('modMultAnimationContinuous', 'var')
  modMultAnimationContinuous = strcmp(modMultFigureVisible, 'on');
end
% Number of points on the circle; the chord envelope sharpens with more points.
if ~exist('modMultPointCount', 'var')
  modMultPointCount = 900;
end
if ~exist('modMultStartMultiplier', 'var')
  modMultStartMultiplier = 2.0;
end
if ~exist('modMultEndMultiplier', 'var')
  modMultEndMultiplier = 62.0;
end

modMultIndices = (0:modMultPointCount - 1).';
modMultAngles = 2 * pi * modMultIndices / modMultPointCount;
modMultPointX = cos(modMultAngles);
modMultPointY = sin(modMultAngles);
modMultFrameCount = max(2, modMultAnimationFrames);
modMultMultipliers = linspace(modMultStartMultiplier, ...
  modMultEndMultiplier, modMultFrameCount);

modMultFigure = figure('Name', 'Modular multiplication circle', ...
  'NumberTitle', 'off', 'Color', [0.03 0.03 0.06], ...
  'Visible', modMultFigureVisible, 'Position', [120 90 820 860]);
modMultAxes = axes('Parent', modMultFigure, ...
  'Position', [0.04 0.09 0.92 0.86], 'Color', [0.03 0.03 0.06]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(modMultAxes);
hold(modMultAxes, 'on');
modMultCircle = plot(modMultAxes, ...
  [modMultPointX; modMultPointX(1)], ...
  [modMultPointY; modMultPointY(1)], ...
  'Color', [0.20 0.24 0.34], 'LineWidth', 1.0);
[modMultChordX, modMultChordY] = modMultChords(modMultIndices, ...
  modMultPointX, modMultPointY, modMultStartMultiplier, modMultPointCount);
modMultLine = plot(modMultAxes, modMultChordX, modMultChordY, ...
  'Color', [0.35 0.85 1.00], 'LineWidth', 0.5);
hold(modMultAxes, 'off');
axis(modMultAxes, 'equal');
axis(modMultAxes, 'off');
set(modMultAxes, 'XLim', [-1.08 1.08], 'YLim', [-1.08 1.08]);
modMultTitle = title(modMultAxes, '', 'Color', [0.90 0.94 1.00], ...
  'FontSize', 13);

modMultInfoText = uicontrol('Parent', modMultFigure, 'Style', 'text', ...
  'String', 'warming up...', 'Position', [20 14 760 22], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.03 0.03 0.06], ...
  'ForegroundColor', [0.70 0.82 0.96], 'FontSize', 10);

if modMultAnimationEnabled
  while isgraphics(modMultFigure)
    for modMultFrameIndex = 1:modMultFrameCount
      if ~isgraphics(modMultFigure)
        break
      end
      try
        modMultMultiplier = modMultMultipliers(modMultFrameIndex);
        modMultClock = tic();
        [modMultChordX, modMultChordY] = modMultChords(modMultIndices, ...
          modMultPointX, modMultPointY, modMultMultiplier, ...
          modMultPointCount);
        % Cycle the chord colour through the spectrum as the multiplier grows.
        set(modMultLine, 'XData', modMultChordX, 'YData', modMultChordY, ...
          'Color', modMultHueToRgb(mod(modMultMultiplier / 10, 1)));
        set(modMultTitle, 'String', sprintf(...
          'n = %d points, multiplier k = %.2f', ...
          modMultPointCount, modMultMultiplier));
        modMultElapsed = toc(modMultClock);
        if modMultElapsed > 0
          set(modMultInfoText, 'String', sprintf(...
            '%d chords recomputed in %.1f ms | %.2f M chords/s', ...
            modMultPointCount, 1e3 * modMultElapsed, ...
            modMultPointCount / modMultElapsed / 1e6));
        end
        drawnow();
      catch
        % The figure was closed mid-frame; stop the animation cleanly.
        break
      end
      if modMultFramePause > 0
        pause(modMultFramePause);
      end
    end
    if ~modMultAnimationContinuous || ~isgraphics(modMultFigure)
      break
    end
  end
end
%=============================================================================
function [chordX, chordY] = modMultChords(indices, pointX, pointY, ...
  multiplier, pointCount)
  % Vectorised chords: each point i connects to point (k*i mod n).
  targets = mod(round(multiplier * indices), pointCount) + 1;
  sources = indices + 1;
  separator = nan(numel(indices), 1);
  chordX = reshape([pointX(sources) pointX(targets) separator].', [], 1);
  chordY = reshape([pointY(sources) pointY(targets) separator].', [], 1);
end
%=============================================================================
function rgb = modMultHueToRgb(hue)
  % Minimal HSV-to-RGB for a fully saturated, bright chord colour.
  sector = floor(hue * 6);
  fraction = hue * 6 - sector;
  low = 0.25;
  rising = 0.25 + 0.75 * fraction;
  falling = 1.0 - 0.75 * fraction;
  switch mod(sector, 6)
    case 0
      rgb = [1.0 rising low];
    case 1
      rgb = [falling 1.0 low];
    case 2
      rgb = [low 1.0 rising];
    case 3
      rgb = [low falling 1.0];
    case 4
      rgb = [rising low 1.0];
    otherwise
      rgb = [1.0 low falling];
  end
end
%=============================================================================
