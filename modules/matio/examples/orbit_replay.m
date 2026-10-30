%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Save a 3-D orbit to a MAT-file, load it back and replay it as a comet.
if ~exist('matioFigureVisible', 'var')
  matioFigureVisible = 'on';
end
if ~exist('matioAnimationEnabled', 'var')
  matioAnimationEnabled = true;
end
if ~exist('matioFramePause', 'var')
  matioFramePause = 0;
end
if ~exist('matioAnimationContinuous', 'var')
  matioAnimationContinuous = strcmp(matioFigureVisible, 'on');
end
if ~exist('matioSampleCount', 'var')
  matioSampleCount = 4000;
end

% Build a precessing rose orbit and save it to a MAT-file as one matrix.
matioTheta = linspace(0, 24 * pi, matioSampleCount);
matioRadius = 1 + 0.5 * cos(5 * matioTheta / 6);
matioOrbit = [(matioRadius .* cos(matioTheta)).', ...
  (matioRadius .* sin(matioTheta)).', (0.5 * sin(3 * matioTheta)).'];
matioFile = [tempname(), '.mat'];
savemat(matioFile, 'matioOrbit');
% Load the orbit straight back from the MAT-file and replay only that.
matioLoaded = loadmat(matioFile);
matioPath = matioLoaded.matioOrbit;

matioFigure = figure('Name', 'MAT-file orbit replay', 'NumberTitle', 'off', ...
  'Color', [0.04 0.05 0.09], 'Visible', matioFigureVisible, ...
  'Position', [140 70 800 800]);
matioAxes = axes('Parent', matioFigure, 'Position', [0.03 0.05 0.94 0.90], ...
  'Color', [0.04 0.05 0.09]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(matioAxes);
hold(matioAxes, 'on');
matioTrail = plot3(matioAxes, matioPath(1, 1), matioPath(1, 2), ...
  matioPath(1, 3), 'Color', [0.35 0.85 1.00], 'LineWidth', 1.4);
matioHead = plot3(matioAxes, matioPath(1, 1), matioPath(1, 2), ...
  matioPath(1, 3), 'o', 'MarkerFaceColor', [1.00 0.78 0.22], ...
  'MarkerEdgeColor', 'none', 'MarkerSize', 10, 'LineStyle', 'none');
hold(matioAxes, 'off');
axis(matioAxes, 'equal');
axis(matioAxes, 'off');
set(matioAxes, 'XLim', [-1.7 1.7], 'YLim', [-1.7 1.7], 'ZLim', [-0.8 0.8]);
title(matioAxes, 'Orbit replayed from a MAT-file', ...
  'Color', [0.92 0.94 0.98], 'FontSize', 13);
matioInfo = uicontrol('Parent', matioFigure, 'Style', 'text', 'String', ...
  sprintf('%d samples loaded with loadmat from a MAT-file', ...
  matioSampleCount), 'Position', [16 8 700 20], 'HorizontalAlignment', ...
  'left', 'BackgroundColor', [0.04 0.05 0.09], 'ForegroundColor', ...
  [0.76 0.84 0.96], 'FontSize', 10);

matioTrailLength = 600;
if matioAnimationEnabled
  matioFrame = 0;
  while isgraphics(matioFigure)
    matioFrame = matioFrame + 1;
    headIndex = mod(matioFrame * 6, matioSampleCount) + 1;
    if ~matioAnimationContinuous && matioFrame * 6 > matioSampleCount
      break
    end
    if ~isgraphics(matioFigure)
      break
    end
    try
      first = max(1, headIndex - matioTrailLength);
      set(matioTrail, 'XData', matioPath(first:headIndex, 1), 'YData', ...
        matioPath(first:headIndex, 2), 'ZData', matioPath(first:headIndex, 3));
      set(matioHead, 'XData', matioPath(headIndex, 1), 'YData', ...
        matioPath(headIndex, 2), 'ZData', matioPath(headIndex, 3));
      view(matioAxes, matioFrame * 0.6, 28);
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if matioFramePause > 0
      pause(matioFramePause);
    end
  end
end
%=============================================================================
