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
close all
%=============================================================================
f = figure('Visible', 'on', 'Position', [120 120 560 420], 'Color', [1 1 1]);
x = linspace(0, 2 * pi, 400);
for k = 1:4
  clf(f);
  ax = axes('Parent', f);
  plot(ax, x, sin(x + k / 3), 'LineWidth', 2);
  grid(ax, 'on');
  title(ax, ['render sync ', int2str(k)]);
  drawnow();
  frame = getframe(f);
  render_assert_not_blank(frame.cdata);
end
%=============================================================================
clf(f);
ax = axes('Parent', f);
timerStart = tic();
for k = 1:25
  plot(ax, x, cos(x + k / 10), 'LineWidth', 1);
  drawnow('limitrate');
end
elapsed = toc(timerStart);
asserts.istrue(elapsed < 30);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
