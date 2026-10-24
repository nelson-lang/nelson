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
% An image() with a zero-valued (double) CData and direct mapping must take the
% first colormap colour, not the last. The 1-based direct index of 0 maps to -1
% in 0-based form; clamping that through the unsigned index type wrapped it to
% the last colormap row, so a zero image rendered white (the top of a gray map)
% instead of black (the bottom). The dark overlays several demos stack this way
% then looked bright.
%=============================================================================
f = figure('Color', 'w', 'Visible', 'off', 'Position', [100 100 150 150]);
colormap(gray);                 % row 1 = black, last row = white
image(zeros(30));               % double zeros, direct mapping
axis off;
drawnow();
%=============================================================================
png = [tempdir(), 'test_image_zero_cdata_direct.png'];
print(f, png, '-dpng');
img = imread(png);
h = size(img, 1);
w = size(img, 2);
center = double(reshape(img(round(h / 2), round(w / 2), 1:3), 1, 3));
% The zero image must be the first (black) colormap colour, not white.
asserts.istrue(max(center) < 40);
close(f);
%=============================================================================
