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
% The adv-cli rasterizer draws the uislider scale (track, ticks, numeric labels)
% and a thumb; the range variant adds a SECOND thumb at the high value.
%=============================================================================
figW = 360;
figH = 140;
fig = uifigure('Visible', 'off', 'Position', [100 100 figW figH]);
%=============================================================================
% A single slider at the low value draws one thumb.
%=============================================================================
s1 = uislider(fig, 'Position', [30 70 300 3], 'Value', 20);
drawnow();
pngSingle = [tempname(), '.png'];
exportapp(fig, pngSingle);
assert_istrue(isfile(pngSingle));
delete(s1);
%=============================================================================
% A range slider [20 70] draws a second thumb at 70: the picture must differ
% from the single-thumb slider at the same low value.
%=============================================================================
rs = uislider(fig, 'range');
rs.Position = [30 70 300 3];
rs.Value = [20 70];
drawnow();
pngRange = [tempname(), '.png'];
exportapp(fig, pngRange);
assert_istrue(isfile(pngRange));
imgSingle = imread(pngSingle);
imgRange = imread(pngRange);
assert_isfalse(isequal(imgSingle, imgRange));
%=============================================================================
% The scale labels/ticks are painted (dark pixels below the track), not a bare
% line.
%=============================================================================
R = imgRange(:, :, 1);
G = imgRange(:, :, 2);
B = imgRange(:, :, 3);
dark = (R < 120) & (G < 120) & (B < 120);
assert_istrue(sum(dark(:)) > 20);
rmfile(pngSingle);
rmfile(pngRange);
delete(fig);
close all
%=============================================================================
