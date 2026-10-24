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
% The adv-cli rasterizer must lay out a multi-line uitextarea Value on separate
% lines, not join every row onto one line.
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 300 160]);
ta = uitextarea(fig, 'Position', [40 40 220 90]);
ta.Value = {'Line one'; 'Line two'; 'Line three'};
drawnow();
pngTa = [tempname(), '.png'];
exportapp(fig, pngTa);
assert_istrue(isfile(pngTa));
img = imread(pngTa);
%=============================================================================
% Three lines occupy a vertical span well beyond a single line of text: a drawer
% that joined them onto one line would keep the text within one line height.
%=============================================================================
R = img(:, :, 1);
G = img(:, :, 2);
B = img(:, :, 3);
darkText = (R < 100) & (G < 100) & (B < 100);
rowsWithText = find(any(darkText, 2));
assert_isfalse(isempty(rowsWithText));
verticalSpan = max(rowsWithText) - min(rowsWithText);
assert_istrue(verticalSpan > 24);
rmfile(pngTa);
delete(fig);
close all
%=============================================================================
