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
% Position read back from a shown figure must be the requested one (the first
% presentation must not leak a transient off-screen or scaled geometry).
%=============================================================================
close all
skip_if_display_cannot_place_figures('figure position readback');
screenSize = get(0, 'ScreenSize');
asserts.isequal(screenSize(1:2), [1 1]);
asserts.istrue(all(screenSize(3:4) >= 320));
asserts.istrue(all(screenSize(3:4) < 20000));
%=============================================================================
requested = [200 200 400 300];
f = figure('Name', 'readback figure', 'Position', requested);
drawnow();
assert_position_near(f.Position, requested);
assert_position_near(f.InnerPosition, requested);
moved = [240 260 380 280];
f.Position = moved;
drawnow();
assert_position_near(f.Position, moved);
close(f);
%=============================================================================
requested = [250 180 300 200];
f = figure('Position', requested);
ax = axes('Parent', f);
plot(ax, 1:10);
drawnow();
assert_position_near(f.Position, requested);
close(f);
%=============================================================================
requested = [220 210 400 300];
fig = uifigure('Name', 'readback uifigure', 'Position', requested);
drawnow();
assert_position_near(fig.Position, requested);
close(fig);
%=============================================================================
requested = [300 220 360 240];
f = figure('Position', requested);
drawnow();
f.WindowState = 'minimized';
drawnow();
assert_position_near(f.Position, requested);
f.WindowState = 'normal';
drawnow();
asserts.isequal(f.WindowState, 'normal');
assert_position_near(f.Position, requested);
close(f);
%=============================================================================
function assert_position_near(position, expected)
  % macOS window manager can round the content-area height by up to 4 px
  tolerance = 4;
  asserts.istrue(all(abs(position - expected) <= tolerance), ...
    sprintf('Position [%s] expected near [%s]', num2str(position), num2str(expected)));
end
%=============================================================================
