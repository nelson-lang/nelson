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
skip_if_portable_graphics_backend('native figure windows are not available.');
% A figure window that cannot be painted (placed off every screen, minimized)
% must not make each drawnow wait for a frame that never comes: an animation
% loop kept running at the render sync timeout (about 4 frames per second)
% instead of its compute speed.
%=============================================================================
close all
screenSize = get(0, 'ScreenSize');
offScreen = [screenSize(3) + 2000, 100, 400, 300];
f = figure('Name', 'drawnow off-screen', 'Position', offScreen);
ax = axes('Parent', f);
im = image('Parent', ax, 'CData', rand(64, 128, 3));
drawnow();
frames = 12;
t = tic();
for k = 1:frames
  set(im, 'CData', rand(64, 128, 3));
  drawnow();
end
elapsedPerFrame = toc(t) / frames;
close(f);
% 250 ms per frame before the fix, a few milliseconds after it.
assert_istrue(elapsedPerFrame < 0.12);
%=============================================================================
% The same figure on screen keeps presenting its frames.
f = figure('Name', 'drawnow on-screen', 'Position', [100 100 400 300]);
ax = axes('Parent', f);
im = image('Parent', ax, 'CData', rand(64, 128, 3));
drawnow();
t = tic();
for k = 1:frames
  set(im, 'CData', rand(64, 128, 3));
  drawnow();
end
elapsedPerFrame = toc(t) / frames;
close(f);
assert_istrue(elapsedPerFrame < 0.12);
%=============================================================================
