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
% <--SEQUENTIAL TEST REQUIRED-->
% <--RELEASE ONLY-->
%=============================================================================
skip_if_portable_graphics_backend('native figure windows are not available.');
% getframe on a figure window that can never be painted on screen (placed off
% every screen, or created with Visible 'off') must force an offscreen render
% and return a frame, instead of waiting for a paint event that never comes.
% Each capture must return in a bounded time; before the render-sync guard a
% capture stalled on the render sync timeout (or hung indefinitely).
%=============================================================================
close all
screenSize = get(0, 'ScreenSize');
offScreen = [screenSize(3) + 2000, 100, 400, 300];
f = figure('Name', 'getframe off-screen', 'Position', offScreen);
ax = axes('Parent', f);
surf(ax, peaks());
captures = 6;
t = tic();
for k = 1:captures
  frame = getframe(f);
end
elapsedPerCapture = toc(t) / captures;
ratio = f.DevicePixelRatio;
framePosition = f.Position;
close(f);
% The frame is a real rendered image, not a blank timeout frame.
assert_isequal(size(frame.cdata), [round(framePosition(4) * ratio), round(framePosition(3) * ratio), 3]);
assert_isequal(class(frame.cdata), 'uint8');
assert_istrue(max(frame.cdata(:)) > min(frame.cdata(:)));
% A full render sync timeout is 0.25 s to 1 s per capture; the offscreen render
% returns in a few milliseconds.
assert_istrue(elapsedPerCapture < 0.2);
%=============================================================================
% A figure created Visible 'off' is captured the same bounded way.
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
ax = axes('Parent', f);
surf(ax, peaks());
t = tic();
for k = 1:captures
  frame = getframe(f);
end
elapsedPerCapture = toc(t) / captures;
close(f);
assert_istrue(max(frame.cdata(:)) > min(frame.cdata(:)));
assert_istrue(elapsedPerCapture < 0.2);
%=============================================================================
