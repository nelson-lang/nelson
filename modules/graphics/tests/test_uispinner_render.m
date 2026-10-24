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
% The adv-cli rasterizer must draw the uispinner up/down stepper arrows in the
% right column of the field, not just a plain value box.
%=============================================================================
figW = 220;
figH = 80;
fig = uifigure('Visible', 'off', 'Position', [100 100 figW figH]);
spx = 20;
spy = 25;
spw = 180;
sph = 30;
sp = uispinner(fig, 'Value', 8, 'Limits', [0 20], 'Position', [spx spy spw sph]);
drawnow();
pngSpin = [tempname(), '.png'];
exportapp(fig, pngSpin);
assert_istrue(isfile(pngSpin));
img = imread(pngSpin);
%=============================================================================
% The stepper arrows add dark pixels in the right column of the field, which a
% plain value box (no steppers) would not have.
%=============================================================================
stepperW = min(18, spw * 0.3);
col0 = round(spx + spw - stepperW) + 1;
col1 = spx + spw;
% Nelson positions are bottom-left; the exported image is top-left origin.
row0 = round(figH - (spy + sph)) + 1;
row1 = round(figH - spy);
R = img(:, :, 1);
G = img(:, :, 2);
B = img(:, :, 3);
dark = (R < 150) & (G < 150) & (B < 150);
region = dark(row0:row1, col0:col1);
assert_istrue(sum(region(:)) > 4);
rmfile(pngSpin);
delete(fig);
close all
%=============================================================================
