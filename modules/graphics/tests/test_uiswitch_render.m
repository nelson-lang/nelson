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
% The adv-cli software rasterizer must draw the uiswitch as a two-state toggle
% (dark track + sliding white knob + the two Items), not a plain box showing a
% single label. A plain box would be independent of Value.
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 260 120]);
sw = uiswitch(fig, 'Position', [80 45 100 32]);
%=============================================================================
% The knob slides and the pill colour changes with the state: Off and On differ.
%=============================================================================
sw.Value = 'Off';
drawnow();
pngOff = [tempname(), '.png'];
exportapp(fig, pngOff);
assert_istrue(isfile(pngOff));
sw.Value = 'On';
drawnow();
pngOn = [tempname(), '.png'];
exportapp(fig, pngOn);
assert_istrue(isfile(pngOn));
imgOff = imread(pngOff);
imgOn = imread(pngOn);
assert_isfalse(isequal(imgOff, imgOn));
%=============================================================================
% The dark toggle track is painted (a plain white/grey box has no mid-grey track
% band of this darkness).
%=============================================================================
R = imgOn(:, :, 1);
G = imgOn(:, :, 2);
B = imgOn(:, :, 3);
track = (R > 90) & (R < 135) & (G > 90) & (G < 135) & (B > 90) & (B < 135);
assert_istrue(sum(track(:)) > 20);
rmfile(pngOff);
rmfile(pngOn);
delete(fig);
close all
%=============================================================================
