%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
tic()
rows = 900;
cols = 1400;
x = repmat(0:(cols - 1), rows, 1);
y = repmat((0:(rows - 1))', 1, cols);
img = uint8(zeros(rows, cols, 3));
img(:, :, 1) = uint8(mod(x, 256));
img(:, :, 2) = uint8(mod(y, 256));
img(:, :, 3) = uint8(mod(x + y, 256));
clear x y;

f = figure('Visible', 'off');
f.Color = [1, 1, 1];
h = imshow(img);
asserts.istrue(isgraphics(h, 'image'));
drawnow();
toc()
close(f);
%=============================================================================
