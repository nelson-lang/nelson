%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Morph a Lissajous figure while two tones let you hear the frequency ratio.
if ~exist('lissajousFigureVisible', 'var')
  lissajousFigureVisible = 'on';
end
if ~exist('lissajousAnimationEnabled', 'var')
  lissajousAnimationEnabled = true;
end
if ~exist('lissajousAnimationFrames', 'var')
  lissajousAnimationFrames = 600;
end
if ~exist('lissajousFramePause', 'var')
  lissajousFramePause = 0;
end
if ~exist('lissajousAnimationContinuous', 'var')
  lissajousAnimationContinuous = strcmp(lissajousFigureVisible, 'on');
end
if ~exist('lissajousFreqA', 'var')
  lissajousFreqA = 3;
end
if ~exist('lissajousFreqB', 'var')
  lissajousFreqB = 2;
end
if ~exist('lissajousSoundEnabled', 'var')
  lissajousSoundEnabled = strcmp(lissajousFigureVisible, 'on');
end

lissajousT = linspace(0, 2 * pi, 1400);
lissajousX = sin(lissajousFreqA * lissajousT);
lissajousPhase = 0;
lissajousY = sin(lissajousFreqB * lissajousT + lissajousPhase);

lissajousFigure = figure('Name', 'Lissajous sound', 'NumberTitle', 'off', ...
  'Color', [0.04 0.05 0.08], 'Visible', lissajousFigureVisible, ...
  'Position', [140 90 820 840]);
lissajousAxes = axes('Parent', lissajousFigure, ...
  'Position', [0.07 0.09 0.86 0.84], 'Color', [0.04 0.05 0.08]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(lissajousAxes);
hold(lissajousAxes, 'on');
lissajousCurve = plot(lissajousAxes, lissajousX, lissajousY, ...
  'Color', [0.35 0.85 1.00], 'LineWidth', 1.6);
lissajousHead = plot(lissajousAxes, lissajousX(1), lissajousY(1), 'o', ...
  'MarkerSize', 10, 'MarkerFaceColor', [1.00 0.78 0.22], ...
  'MarkerEdgeColor', 'none', 'LineStyle', 'none');
hold(lissajousAxes, 'off');
axis(lissajousAxes, 'equal');
set(lissajousAxes, 'XLim', [-1.15 1.15], 'YLim', [-1.15 1.15], ...
  'XColor', [0.6 0.66 0.76], 'YColor', [0.6 0.66 0.76]);
lissajousTitle = title(lissajousAxes, '', 'Color', [0.92 0.94 0.98], ...
  'FontSize', 13);
lissajousInfoText = uicontrol('Parent', lissajousFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 10 760 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.04 0.05 0.08], 'ForegroundColor', [0.72 0.82 0.96], ...
  'FontSize', 10);

% Play a chord of the two frequencies so the ratio can be heard.
lissajousPlayer = lissajousPlayChord(lissajousSoundEnabled, ...
  lissajousFreqA, lissajousFreqB);

if lissajousAnimationEnabled
  lissajousHeadIndex = 1;
  lissajousFrame = 0;
  while isgraphics(lissajousFigure)
    lissajousFrame = lissajousFrame + 1;
    if lissajousFrame > lissajousAnimationFrames && ...
        ~lissajousAnimationContinuous
      break
    end
    if ~isgraphics(lissajousFigure)
      break
    end
    try
      lissajousPhase = lissajousPhase + 0.03;
      lissajousY = sin(lissajousFreqB * lissajousT + lissajousPhase);
      lissajousHeadIndex = mod(lissajousHeadIndex + 18, numel(lissajousT)) + 1;
      set(lissajousCurve, 'YData', lissajousY);
      set(lissajousHead, 'XData', lissajousX(lissajousHeadIndex), ...
        'YData', lissajousY(lissajousHeadIndex));
      set(lissajousTitle, 'String', sprintf(...
        'Lissajous a:b = %d:%d, phase = %.2f rad', ...
        lissajousFreqA, lissajousFreqB, mod(lissajousPhase, 2 * pi)));
      if isgraphics(lissajousInfoText)
        set(lissajousInfoText, 'String', sprintf(...
          'x = sin(%d t), y = sin(%d t + phase) | simple ratio = consonant chord', ...
          lissajousFreqA, lissajousFreqB));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if lissajousFramePause > 0
      pause(lissajousFramePause);
    end
  end
end
lissajousStopPlayer(lissajousPlayer);
%=============================================================================
function player = lissajousPlayChord(enabled, freqA, freqB)
  player = [];
  if ~enabled || exist('audioplayer', 'builtin') == 0
    return
  end
  try
    rate = 22050;
    t = (0:round(4.0 * rate) - 1) / rate;
    baseHz = 110;
    envelope = min(1, 10 * t) .* exp(-t * 0.35);
    tone = sin(2 * pi * baseHz * freqA * t) + ...
      sin(2 * pi * baseHz * freqB * t);
    sample = 0.3 * (envelope .* tone);
    player = audioplayer(sample(:), rate);
    play(player);
  catch
    player = [];
  end
end
%=============================================================================
function lissajousStopPlayer(player)
  if isempty(player)
    return
  end
  try
    if isvalid(player)
      stop(player);
    end
  catch
  end
end
%=============================================================================
