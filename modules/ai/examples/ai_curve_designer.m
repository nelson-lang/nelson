%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Ask a local AI model for curve parameters and animate the curve it designs.
if ~exist('aiCurveFigureVisible', 'var')
  aiCurveFigureVisible = 'on';
end
if ~exist('aiCurveAnimationEnabled', 'var')
  aiCurveAnimationEnabled = true;
end
if ~exist('aiCurveAnimationFrames', 'var')
  aiCurveAnimationFrames = 700;
end
if ~exist('aiCurveFramePause', 'var')
  aiCurveFramePause = 0;
end
if ~exist('aiCurveAnimationContinuous', 'var')
  aiCurveAnimationContinuous = strcmp(aiCurveFigureVisible, 'on');
end

% Ask the model for three frequencies; fall back to defaults if it is absent.
aiCurvePrompt = ['Give exactly three integers between 2 and 7 separated ', ...
  'by commas for a pretty Lissajous curve. Reply with only the numbers.'];
aiCurveParameters = [3 2 5];
aiCurveSource = 'default parameters (no AI server reachable)';
try
  aiCurveReply = aiask(aiCurvePrompt);
  aiCurveNumbers = str2double(regexp(char(aiCurveReply), ...
    '\d+', 'match'));
  aiCurveNumbers = aiCurveNumbers(isfinite(aiCurveNumbers));
  if numel(aiCurveNumbers) >= 3
    aiCurveParameters = min(max(round(aiCurveNumbers(1:3)), 2), 7);
    aiCurveSource = 'parameters chosen by the local AI model';
  end
catch
end

aiCurveT = linspace(0, 2 * pi, 1600);
aiCurveX = sin(aiCurveParameters(1) * aiCurveT);
aiCurvePhase = 0;
aiCurveY = sin(aiCurveParameters(2) * aiCurveT + aiCurvePhase);

aiCurveFigure = figure('Name', 'AI curve designer', 'NumberTitle', 'off', ...
  'Color', [0.04 0.05 0.09], 'Visible', aiCurveFigureVisible, ...
  'Position', [140 80 820 840]);
aiCurveAxes = axes('Parent', aiCurveFigure, 'Position', ...
  [0.06 0.09 0.88 0.84], 'Color', [0.04 0.05 0.09]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(aiCurveAxes);
aiCurveLine = plot(aiCurveAxes, aiCurveX, aiCurveY, 'Color', ...
  [0.35 0.85 1.00], 'LineWidth', 1.8);
axis(aiCurveAxes, 'equal');
axis(aiCurveAxes, 'off');
set(aiCurveAxes, 'XLim', [-1.15 1.15], 'YLim', [-1.15 1.15]);
title(aiCurveAxes, sprintf('Lissajous %d:%d from a local AI model', ...
  aiCurveParameters(1), aiCurveParameters(2)), 'Color', ...
  [0.92 0.94 0.98], 'FontSize', 13);
aiCurveInfo = uicontrol('Parent', aiCurveFigure, 'Style', 'text', ...
  'String', aiCurveSource, 'Position', [16 10 760 20], ...
  'HorizontalAlignment', 'left', 'BackgroundColor', [0.04 0.05 0.09], ...
  'ForegroundColor', [0.76 0.84 0.96], 'FontSize', 10);

if aiCurveAnimationEnabled
  aiCurveFrame = 0;
  while isgraphics(aiCurveFigure)
    aiCurveFrame = aiCurveFrame + 1;
    if aiCurveFrame > aiCurveAnimationFrames && ~aiCurveAnimationContinuous
      break
    end
    if ~isgraphics(aiCurveFigure)
      break
    end
    try
      aiCurvePhase = aiCurvePhase + 0.03;
      aiCurveY = sin(aiCurveParameters(2) * aiCurveT + aiCurvePhase);
      set(aiCurveLine, 'YData', aiCurveY);
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if aiCurveFramePause > 0
      pause(aiCurveFramePause);
    end
  end
end
%=============================================================================
