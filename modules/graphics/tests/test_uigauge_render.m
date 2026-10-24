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
% The adv-cli software rasterizer must draw the uigauge dial (scale arc, filled
% portion and needle), not a blank container. A blank box would be independent
% of Value and carry no accent colour.
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 240 240]);
g = uigauge(fig);
g.Position = [30 30 180 180];
g.Limits = [0 100];
%=============================================================================
% The needle and filled arc track Value: the empty and full dials must differ.
%=============================================================================
g.Value = 0;
drawnow();
pngLow = [tempname(), '.png'];
exportapp(fig, pngLow);
assert_istrue(isfile(pngLow));
g.Value = 100;
drawnow();
pngHigh = [tempname(), '.png'];
exportapp(fig, pngHigh);
assert_istrue(isfile(pngHigh));
imgLow = imread(pngLow);
imgHigh = imread(pngHigh);
assert_isfalse(isequal(imgLow, imgHigh));
%=============================================================================
% The red value needle is actually painted (a blank grey box has no strongly
% red pixel).
%=============================================================================
R = imgHigh(:, :, 1);
G = imgHigh(:, :, 2);
B = imgHigh(:, :, 3);
reddish = (R > 150) & (G < 90) & (B < 90);
assert_istrue(any(reddish(:)));
%=============================================================================
rmfile(pngLow);
rmfile(pngHigh);
delete(fig);
close all
%=============================================================================
