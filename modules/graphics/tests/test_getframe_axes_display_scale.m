%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% The frame of an axes is the block of the figure frame the axes covers, whatever the
% display scale, for a shown or a hidden figure, and whatever holds the axes.
%=============================================================================
figureSize = [500 300];
axesPosition = [51 41 300 200];
visible = {'on', 'off'};
for k = 1:2
  f = figure('Visible', visible{k}, 'Position', [100 100 figureSize], 'Color', [1 1 1]);
  ax = axes('Parent', f, 'Units', 'pixels', 'Position', axesPosition, 'Color', [1 0 0], ...
    'XTick', [], 'YTick', [], 'XLim', [0 1], 'YLim', [0 1]);
  drawnow();
  figureFrame = getframe(f);
  axesFrame = getframe(ax);
  frameSize = [size(figureFrame.cdata, 2), size(figureFrame.cdata, 1)];
  scale = frameSize ./ figureSize;
  expectedSize = round((axesPosition(1:2) - 1 + axesPosition(3:4)) .* scale) ...
    - round((axesPosition(1:2) - 1) .* scale);
  assert_isequal([size(axesFrame.cdata, 2), size(axesFrame.cdata, 1)], expectedSize);
  % the figure background stays out of the frame, the axes fills it
  cdata = axesFrame.cdata;
  background = all(cdata > 240, 3);
  assert_isfalse(any(background(:)));
  axesColor = cdata(:, :, 1) > 200 & cdata(:, :, 2) < 60 & cdata(:, :, 3) < 60;
  assert_istrue(sum(axesColor(:)) > 0.95 * numel(axesColor));
  % the Units of the axes are left as they were
  assert_isequal(ax.Units, 'pixels');
  close(f);
end
%=============================================================================
% an axes in a panel, in nested panels, in a tab: its Position is relative to its container
containers = {'panel', 'titled panel', 'nested panels', 'tab'};
for k = 1:numel(containers)
  f = figure('Visible', 'off', 'Position', [100 100 figureSize], 'Color', [1 1 1]);
  switch containers{k}
    case 'panel'
      parent = uipanel('Parent', f, 'Units', 'pixels', 'Position', [101 51 300 200], ...
        'BorderType', 'none', 'BackgroundColor', [0 1 0]);
    case 'titled panel'
      parent = uipanel('Parent', f, 'Units', 'pixels', 'Position', [101 51 300 200], ...
        'Title', 'Title', 'BackgroundColor', [0 1 0]);
    case 'nested panels'
      outer = uipanel('Parent', f, 'Units', 'pixels', 'Position', [51 31 400 250], ...
        'Title', 'Outer', 'BackgroundColor', [0 1 0]);
      parent = uipanel('Parent', outer, 'Units', 'pixels', 'Position', [31 21 300 200], ...
        'BorderType', 'none', 'BackgroundColor', [0 1 0]);
    case 'tab'
      group = uitabgroup('Parent', f);
      parent = uitab('Parent', group, 'Title', 'A', 'BackgroundColor', [0 1 0]);
  end
  ax = axes('Parent', parent, 'Units', 'pixels', 'Position', [21 31 200 120], ...
    'Color', [1 0 0], 'XTick', [], 'YTick', [], 'XLim', [0 1], 'YLim', [0 1]);
  drawnow();
  assert_istrue(axesFillsItsFrame(f, ax, [200 120], figureSize));
  close(f);
end
%=============================================================================
% an axes of a tiled layout
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
t = tiledlayout(f, 2, 2);
ax = nexttile(t, 4);
set(ax, 'Color', [1 0 0], 'XTick', [], 'YTick', []);
drawnow();
tilePosition = getpixelposition(ax);
assert_istrue(axesFillsItsFrame(f, ax, tilePosition(3:4), [560 420]));
% the figure is captured like a figure that holds its axes itself
tiledFrame = getframe(f);
close(f);
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
axes('Parent', f);
drawnow();
plainFrame = getframe(f);
assert_isequal(size(tiledFrame.cdata), size(plainFrame.cdata));
close(f);
%=============================================================================
% the frame has the size of the axes and shows the axes, not its surroundings
function tf = axesFillsItsFrame(f, ax, axesSize, figureSize)
  figureFrame = getframe(f);
  axesFrame = getframe(ax);
  cdata = axesFrame.cdata;
  scale = [size(figureFrame.cdata, 2), size(figureFrame.cdata, 1)] ./ figureSize;
  frameSize = [size(cdata, 2), size(cdata, 1)];
  axesColor = cdata(:, :, 1) > 200 & cdata(:, :, 2) < 60 & cdata(:, :, 3) < 60;
  tf = all(abs(frameSize - axesSize .* scale) <= 1) && sum(axesColor(:)) > 0.95 * numel(axesColor);
end
%=============================================================================
