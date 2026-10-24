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
skip_if_portable_graphics_backend('native figure window placement is not available.');
skip_if_display_cannot_place_figures('native figure placement');
% The drawing area of a figure window must really sit where Position asks, as
% the native window reports it. A read-back of Position cannot see a constant
% offset shared by placement and read-back (the window one title bar too low).
%=============================================================================
close all
screenSize = get(0, 'ScreenSize');
asserts.istrue(all(screenSize(3:4) >= 640));
%=============================================================================
% no menu bar nor toolbar
requested = [200 120 400 300];
fig = uifigure('Name', 'native placement uifigure', 'Position', requested);
assert_native_near(fig, requested);
moved = [260 150 360 240];
fig.Position = moved;
assert_native_near(fig, moved);
close(fig);
%=============================================================================
% menu bar and toolbar above the drawing area
requested = [220 130 420 280];
f = figure('Name', 'native placement figure', 'MenuBar', 'figure', 'ToolBar', 'figure', ...
  'Position', requested);
assert_native_near(f, requested);
moved = [180 170 380 260];
f.Position = moved;
assert_native_near(f, moved);
close(f);
%=============================================================================
% minimized then restored
requested = [300 140 360 240];
f = figure('Position', requested);
assert_native_near(f, requested);
f.WindowState = 'minimized';
drawnow();
f.WindowState = 'normal';
assert_native_near(f, requested);
close(f);
%=============================================================================
function assert_native_near(f, expected)
  % macOS window manager can round the content-area height by up to 4 px
  tolerance = 4;
  % the native window may settle a few event-loop turns after drawnow
  native = [];
  for k = 1:40
    drawnow();
    native = __figure_native_geometry__(f);
    if ~isempty(native) && all(abs(native - expected) <= tolerance)
      break
    end
    pause(0.05);
  end
  asserts.isfalse(isempty(native), 'no native figure window');
  asserts.istrue(all(abs(native - expected) <= tolerance), ...
    sprintf('drawing area on screen [%s] expected near [%s]', num2str(native), num2str(expected)));
end
%=============================================================================
