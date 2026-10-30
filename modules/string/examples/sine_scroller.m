%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Scroll a text banner with a sine wobble, building each frame from strings.
if ~exist('scrollerFigureVisible', 'var')
  scrollerFigureVisible = 'on';
end
if ~exist('scrollerAnimationEnabled', 'var')
  scrollerAnimationEnabled = true;
end
if ~exist('scrollerAnimationFrames', 'var')
  scrollerAnimationFrames = 1200;
end
if ~exist('scrollerFramePause', 'var')
  scrollerFramePause = 0;
end
if ~exist('scrollerAnimationContinuous', 'var')
  scrollerAnimationContinuous = strcmp(scrollerFigureVisible, 'on');
end
if ~exist('scrollerWindow', 'var')
  scrollerWindow = 32;
end

% Build the marquee text from pieces with string functions, then pad it so the
% message scrolls fully off screen before repeating.
scrollerParts = {'NELSON', 'GREETINGS FROM THE STRING MODULE', ...
  'sprintf . strjoin . pad . regexp', 'ENJOY THE SINE SCROLLER'};
scrollerMessage = strjoin(scrollerParts, '   ***   ');
scrollerMessage = char(pad(scrollerMessage, ...
  strlength(scrollerMessage) + scrollerWindow, 'right'));
scrollerLength = numel(scrollerMessage);

scrollerFigure = figure('Name', 'Sine scroller', 'NumberTitle', 'off', ...
  'Color', [0.02 0.02 0.05], 'Visible', scrollerFigureVisible, ...
  'Position', [100 160 1120 320]);
scrollerAxes = axes('Parent', scrollerFigure, 'Position', [0 0 1 1], ...
  'Color', [0.02 0.02 0.05]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(scrollerAxes);
set(scrollerAxes, 'XLim', [0 scrollerWindow + 1], 'YLim', [-1.2 1.2]);
axis(scrollerAxes, 'off');
scrollerChars = gobjects(1, scrollerWindow);
for columnIndex = 1:scrollerWindow
  scrollerChars(columnIndex) = text(scrollerAxes, columnIndex, 0, ' ', ...
    'FontName', 'Courier New', 'FontSize', 34, 'FontWeight', 'bold', ...
    'HorizontalAlignment', 'center', 'Color', [1 1 1]);
end

if scrollerAnimationEnabled
  scrollerFrame = 0;
  while isgraphics(scrollerFigure)
    scrollerFrame = scrollerFrame + 1;
    if scrollerFrame > scrollerAnimationFrames && ...
        ~scrollerAnimationContinuous
      break
    end
    if ~isgraphics(scrollerFigure)
      break
    end
    try
      offset = mod(scrollerFrame - 1, scrollerLength);
      phase = scrollerFrame * 0.12;
      for columnIndex = 1:scrollerWindow
        sourceIndex = mod(offset + columnIndex - 1, scrollerLength) + 1;
        letter = scrollerMessage(sourceIndex);
        wobble = 0.65 * sin(phase + columnIndex * 0.45);
        hue = mod((columnIndex + scrollerFrame) * 0.02, 1);
        set(scrollerChars(columnIndex), 'String', letter, ...
          'Position', [columnIndex wobble 0], ...
          'Color', scrollerHue(hue));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if scrollerFramePause > 0
      pause(scrollerFramePause);
    end
  end
end
%=============================================================================
function rgb = scrollerHue(hue)
  sector = floor(hue * 6);
  fraction = hue * 6 - sector;
  rising = 0.3 + 0.7 * fraction;
  falling = 1.0 - 0.7 * fraction;
  switch mod(sector, 6)
    case 0
      rgb = [1.0 rising 0.3];
    case 1
      rgb = [falling 1.0 0.3];
    case 2
      rgb = [0.3 1.0 rising];
    case 3
      rgb = [0.3 falling 1.0];
    case 4
      rgb = [rising 0.3 1.0];
    otherwise
      rgb = [1.0 0.3 falling];
  end
end
%=============================================================================
