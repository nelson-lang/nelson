%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Build function handles from text with str2func and animate them live.
if ~exist('explorerFigureVisible', 'var')
  explorerFigureVisible = 'on';
end
if ~exist('explorerAnimationEnabled', 'var')
  explorerAnimationEnabled = true;
end
if ~exist('explorerAnimationFrames', 'var')
  explorerAnimationFrames = 900;
end
if ~exist('explorerFramePause', 'var')
  explorerFramePause = 0;
end
if ~exist('explorerAnimationContinuous', 'var')
  explorerAnimationContinuous = strcmp(explorerFigureVisible, 'on');
end

% Each expression is plain text turned into a callable handle by str2func.
explorerExpressions = {'@(x, k) sin(k * x)', ...
  '@(x, k) cos(k * x) + sin(2 * x) / 2', ...
  '@(x, k) tanh(k * x)', ...
  '@(x, k) sin(k * x) / k'};
explorerHandles = cell(1, numel(explorerExpressions));
for index = 1:numel(explorerExpressions)
  explorerHandles{index} = str2func(explorerExpressions{index});
end
explorerX = linspace(-pi, pi, 600);
explorerFramesPerFunction = 150;

explorerFigure = figure('Name', 'Function handle explorer', ...
  'NumberTitle', 'off', 'Color', [0.05 0.06 0.09], ...
  'Visible', explorerFigureVisible, 'Position', [120 120 980 560]);
explorerAxes = axes('Parent', explorerFigure, ...
  'Position', [0.08 0.14 0.88 0.74], 'Color', [0.05 0.06 0.09]);
% Prevent accidental pan or zoom from disturbing the live animation.
disableDefaultInteractivity(explorerAxes);
explorerCurve = plot(explorerAxes, explorerX, zeros(size(explorerX)), ...
  'Color', [0.35 0.85 1.00], 'LineWidth', 2.0);
set(explorerAxes, 'XColor', [0.6 0.67 0.78], 'YColor', [0.6 0.67 0.78]);
set(explorerAxes, 'XLim', [-pi pi], 'YLim', [-1.6 1.6]);
grid(explorerAxes, 'on');
xlabel(explorerAxes, 'x');
ylabel(explorerAxes, 'f(x)');
explorerTitle = title(explorerAxes, '', 'Color', [0.92 0.94 0.98], ...
  'FontSize', 13, 'Interpreter', 'none');
explorerInfo = uicontrol('Parent', explorerFigure, 'Style', 'text', ...
  'String', '', 'Position', [16 8 820 20], 'HorizontalAlignment', 'left', ...
  'BackgroundColor', [0.05 0.06 0.09], 'ForegroundColor', [0.74 0.82 0.95], ...
  'FontSize', 10);

if explorerAnimationEnabled
  explorerFrame = 0;
  while isgraphics(explorerFigure)
    explorerFrame = explorerFrame + 1;
    if explorerFrame > explorerAnimationFrames && ...
        ~explorerAnimationContinuous
      break
    end
    if ~isgraphics(explorerFigure)
      break
    end
    try
      functionIndex = mod(floor((explorerFrame - 1) / ...
        explorerFramesPerFunction), numel(explorerHandles)) + 1;
      handle = explorerHandles{functionIndex};
      % The parameter k sweeps so the curve stretches and morphs.
      k = 1 + 3.5 * (0.5 + 0.5 * sin(explorerFrame * 0.05));
      y = handle(explorerX, k);
      set(explorerCurve, 'YData', y);
      set(explorerTitle, 'String', sprintf('f(x) = %s', ...
        func2str(handle)));
      if isgraphics(explorerInfo)
        set(explorerInfo, 'String', sprintf(...
          'handle built by str2func | parameter k = %.2f', k));
      end
      drawnow();
    catch
      % The figure was closed mid-frame; stop the animation cleanly.
      break
    end
    if explorerFramePause > 0
      pause(explorerFramePause);
    end
  end
end
%=============================================================================
