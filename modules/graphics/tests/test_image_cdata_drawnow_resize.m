%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
close all
f = figure('Visible', 'on', 'MenuBar', 'none', 'ToolBar', 'none', ...
  'Position', [100 100 360 240]);
ax = axes('Parent', f, 'Position', [0 0 1 1]);
cdata = zeros(18, 24, 3);
imageHandle = image('Parent', ax, 'CData', cdata);
axis(ax, 'image');
axis(ax, 'off');

windowSizes = [360 240; 1000 650];
colors = [1 0 0; 0 0 1];
for index = 1:size(windowSizes, 1)
  set(f, 'Position', [100 100 windowSizes(index, :)]);
  for channel = 1:3
    cdata(:, :, channel) = colors(index, channel);
  end
  set(imageHandle, 'CData', cdata);
  drawnow();

  captured = getframe(f);
  render_assert_not_blank(captured.cdata);
  center = squeeze(captured.cdata(round(end / 2), round(end / 2), :));
  expectedChannel = 1 + 2 * (index - 1);
  assert_istrue(center(expectedChannel) > 200);
  assert_istrue(all(center(setdiff(1:3, expectedChannel)) < 30));
end

cdata = zeros(11, 17, 3);
cdata(:, :, 2) = 1;
set(imageHandle, 'CData', cdata);
drawnow();
captured = getframe(f);
center = squeeze(captured.cdata(round(end / 2), round(end / 2), :));
assert_istrue(center(2) > 200);
assert_istrue(all(center([1 3]) < 30));

cdata8 = uint8(zeros(13, 19, 3));
cdata8(:, :, 1) = uint8(231);
cdata8(:, :, 3) = uint8(47);
set(imageHandle, 'CData', cdata8);
drawnow();
captured = getframe(f);
center = squeeze(captured.cdata(round(end / 2), round(end / 2), :));
assert_istrue(abs(double(center(1)) - 231) < 3);
assert_istrue(center(2) < 3);
assert_istrue(abs(double(center(3)) - 47) < 3);
close(f);
%=============================================================================
