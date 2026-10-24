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
% FaceAlpha 'flat' with per-element AlphaData must apply the per-face
% transparency even when FaceColor is a solid colour (a colorspec), not only
% when the colour itself is data-mapped ('flat'). The surface quads share one
% packed-quad layout for both cases, so the renderer must honour the per-vertex
% alpha regardless of the colour mode.
%=============================================================================
f = figure('Color', 'w', 'Visible', 'off', 'Position', [100 100 300 300]);
A = ones(20, 20);
A(:, 1:10) = 0;   % left half fully transparent, right half opaque
[x, y] = meshgrid(linspace(-3, 3, 20));
surf(x, y, ones(size(x)), 'FaceColor', [0.2 0.8 0.3], 'FaceAlpha', 'flat', ...
  'AlphaData', A, 'EdgeColor', 'none');
view(0, 90);
axis off;
drawnow();
%=============================================================================
img = getframe(f).cdata;
h = size(img, 1);
w = size(img, 2);
% Left quarter (A=0) is transparent: the white figure shows through.
leftPixel = double(reshape(img(round(h / 2), round(w / 4), 1:3), 1, 3));
asserts.istrue(min(leftPixel) > 240);
% Right quarter (A=1) is the opaque green surface.
rightPixel = double(reshape(img(round(h / 2), round(3 * w / 4), 1:3), 1, 3));
asserts.istrue(rightPixel(2) > rightPixel(1) + 30 && rightPixel(2) > rightPixel(3) + 30);
close(f);
%=============================================================================
