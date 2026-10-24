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
% The adv-cli software rasterizer must draw the uiknob dial (body, rim and the
% accent pointer towards Value), not a blank container.
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 220 220]);
kb = uiknob(fig);
kb.Position = [30 30 160 160];
kb.Limits = [0 100];
%=============================================================================
% The pointer tracks Value: the two ends of the dial must differ.
%=============================================================================
kb.Value = 0;
drawnow();
pngLow = [tempname(), '.png'];
exportapp(fig, pngLow);
assert_istrue(isfile(pngLow));
kb.Value = 100;
drawnow();
pngHigh = [tempname(), '.png'];
exportapp(fig, pngHigh);
assert_istrue(isfile(pngHigh));
imgLow = imread(pngLow);
imgHigh = imread(pngHigh);
assert_isfalse(isequal(imgLow, imgHigh));
%=============================================================================
% The red value pointer is actually painted.
%=============================================================================
R = imgHigh(:, :, 1);
G = imgHigh(:, :, 2);
B = imgHigh(:, :, 3);
reddish = (R > 150) & (G < 90) & (B < 90);
assert_istrue(any(reddish(:)));
rmfile(pngLow);
rmfile(pngHigh);
%=============================================================================
% A discrete knob reuses the same drawing (ValueIndex drives the pointer).
%=============================================================================
dk = uiknob(fig, 'discrete');
dk.Position = [30 30 160 160];
dk.Items = {'Low', 'Medium', 'High'};
dk.Value = 'High';
drawnow();
pngDiscrete = [tempname(), '.png'];
exportapp(fig, pngDiscrete);
assert_istrue(isfile(pngDiscrete));
imgDiscrete = imread(pngDiscrete);
Rd = imgDiscrete(:, :, 1);
Gd = imgDiscrete(:, :, 2);
Bd = imgDiscrete(:, :, 3);
reddishDiscrete = (Rd > 150) & (Gd < 90) & (Bd < 90);
assert_istrue(any(reddishDiscrete(:)));
rmfile(pngDiscrete);
delete(fig);
close all
%=============================================================================
