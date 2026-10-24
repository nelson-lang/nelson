%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Display a live, interruptible analog clock animation (Stop to end).
figureHandle = figure('Name', 'Analog clock', 'NumberTitle', 'off', ...
  'Position', [300 200 520 560]);
axesHandle = axes('Parent', figureHandle, 'Position', [0.08 0.14 0.84 0.80]);
hold(axesHandle, 'on');
faceAngle = linspace(0, 2 * pi, 361);
plot(axesHandle, cos(faceAngle), sin(faceAngle), 'Color', [0.15 0.35 0.75], ...
  'LineWidth', 3);
for hourIndex = 1:12
  tickAngle = pi / 2 - hourIndex * pi / 6;
  plot(axesHandle, [0.82 0.94] * cos(tickAngle), ...
    [0.82 0.94] * sin(tickAngle), 'Color', [0.10 0.10 0.12], ...
    'LineWidth', 2);
  text(0.72 * cos(tickAngle), 0.72 * sin(tickAngle), ...
    num2str(hourIndex), 'Parent', axesHandle, 'HorizontalAlignment', 'center');
end
hourHand = plot(axesHandle, [0 0], [0 0.48], 'LineWidth', 5, ...
  'Color', [0.10 0.10 0.12]);
minuteHand = plot(axesHandle, [0 0], [0 0.70], 'LineWidth', 3, ...
  'Color', [0.15 0.35 0.75]);
secondHand = plot(axesHandle, [0 0], [0 0.82], 'LineWidth', 1.5, ...
  'Color', [0.85 0.20 0.20]);
plot(axesHandle, 0, 0, 'ko', 'MarkerFaceColor', 'k');
hold(axesHandle, 'off');
axis(axesHandle, 'equal');
axis(axesHandle, [-1.1 1.1 -1.1 1.1]);
axis(axesHandle, 'off');
stopButton = uicontrol('Parent', figureHandle, 'Style', 'pushbutton', ...
  'String', 'Stop', 'Position', [210 18 100 32], 'UserData', false, ...
  'Callback', @(source, event) set(source, 'UserData', true));

% Tick like a real clock until the user presses "Stop" or closes the window.
while true
  if ~isgraphics(figureHandle) || ~isgraphics(stopButton) || stopButton.UserData
    break
  end
  currentTime = clock();
  seconds = currentTime(6);
  minutes = currentTime(5) + seconds / 60;
  hours = mod(currentTime(4), 12) + minutes / 60;
  secondAngle = pi / 2 - seconds * pi / 30;
  minuteAngle = pi / 2 - minutes * pi / 30;
  hourAngle = pi / 2 - hours * pi / 6;
  set(secondHand, 'XData', [0 0.82 * cos(secondAngle)], ...
    'YData', [0 0.82 * sin(secondAngle)]);
  set(minuteHand, 'XData', [0 0.70 * cos(minuteAngle)], ...
    'YData', [0 0.70 * sin(minuteAngle)]);
  set(hourHand, 'XData', [0 0.48 * cos(hourAngle)], ...
    'YData', [0 0.48 * sin(hourAngle)]);
  title(axesHandle, sprintf('%02d:%02d:%02d', mod(floor(hours), 12), ...
    floor(minutes), floor(seconds)));
  drawnow();
  pause(0.04);
end
%=============================================================================
