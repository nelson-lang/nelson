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
% A semi-transparent surface drawn in front of an opaque one must blend over it,
% not be hidden by it. The software (print/saveas) capture rasterizes each
% surface into its own image: the opaque surface owns a depth buffer, and the
% transparent surface is tested against it per pixel so it is dropped only where
% it falls behind the opaque geometry -- and blended over it where it is in
% front. A regression here paints the opaque surface on top and the transparent
% one disappears (the tunnel demo lost its water this way).
%=============================================================================
f = figure('Color', 'w', 'Visible', 'off', 'Position', [100 100 300 300]);
[x, y] = meshgrid(linspace(-3, 3, 10));
% Opaque red floor at z = 0.
surf(x, y, zeros(size(x)), 'FaceColor', [1 0 0], 'FaceAlpha', 1, 'EdgeColor', 'none');
hold on;
% Half-transparent blue sheet at z = 1, nearer the top-down camera, covering it.
surf(x, y, ones(size(x)), 'FaceColor', [0 0 1], 'FaceAlpha', 0.5, 'EdgeColor', 'none');
view(0, 90);
axis off;
drawnow();
%=============================================================================
png = [tempdir(), 'test_surface_transparent_over_opaque.png'];
print(f, png, '-dpng');
img = imread(png);
h = size(img, 1);
w = size(img, 2);
center = double(reshape(img(round(h / 2), round(w / 2), 1:3), 1, 3));
% The blue sheet is in front and must show through: its contribution lands in
% the blue channel. If the opaque red were drawn on top, blue would be ~0.
asserts.istrue(center(3) > 80);
% The red floor still shows through the 50% sheet, so the result is a blend, not
% a flat blue.
asserts.istrue(center(1) > 80);
close(f);
%=============================================================================
