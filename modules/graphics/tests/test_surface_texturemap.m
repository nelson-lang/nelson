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
close all
reset(groot());
addpath([modulepath('graphics', 'tests')]);
%=============================================================================
% 'texture' is accepted as the shortest unambiguous prefix of 'texturemap'.
f = figure('Visible', 'off');
[X, Y] = meshgrid(linspace(0, 1, 10), linspace(0, 1, 10));
Z = zeros(10, 10);
img = zeros(4, 4, 3);
img(:, 1:2, 1) = 1;   % left half red
img(:, 3:4, 3) = 1;   % right half blue
S = surf(X, Y, Z, img, 'FaceColor', 'texture', 'EdgeColor', 'none');
asserts.isequal(char(S.FaceColor), 'texturemap');
close(f);
%=============================================================================
% 'texturemap' is accepted directly, and an image CData of arbitrary size
% (independent of Z) is kept as-is.
f = figure('Visible', 'off');
S = surf(X, Y, Z, img, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
asserts.isequal(char(S.FaceColor), 'texturemap');
asserts.isequal(size(S.CData), [4 4 3]);
asserts.isequal(size(S.ZData), [10 10]);
close(f);
%=============================================================================
% A set() to 'texturemap' is accepted on an existing surface.
f = figure('Visible', 'off');
S = surf(peaks(8));
set(S, 'FaceColor', 'texturemap');
asserts.isequal(char(S.FaceColor), 'texturemap');
close(f);
%=============================================================================
% The texture is really mapped onto the surface: a left-red / right-blue image
% draws red pixels on the left and blue pixels on the right, not a flat colour.
f = figure('Visible', 'off', 'Color', [1 1 1], 'Position', [100 100 400 400]);
hi = 64;
tex = zeros(hi, hi, 3);
tex(:, 1:hi / 2, 1) = 1;        % left half red
tex(:, hi / 2 + 1:end, 3) = 1;  % right half blue
surf(X, Y, Z, tex, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
view(2);
set(gca, 'YDir', 'normal');
drawnow();
F = getframe(f);
render_assert_not_blank(F.cdata);
r = double(F.cdata(:, :, 1));
g = double(F.cdata(:, :, 2));
b = double(F.cdata(:, :, 3));
redMask = r > 180 & g < 100 & b < 100;
blueMask = b > 180 & r < 100 & g < 100;
asserts.istrue(nnz(redMask) > 500);
asserts.istrue(nnz(blueMask) > 500);
% Orientation: the red half is to the left of the blue half.
[~, redCols] = find(redMask);
[~, blueCols] = find(blueMask);
asserts.istrue(mean(redCols) < mean(blueCols));
close(f);
%=============================================================================
% texturemap keeps the texture crisp on a coarse mesh: a high-resolution 2-cell
% pattern mapped onto a 2-by-2 grid still shows a sharp colour boundary (true
% per-fragment texturing, not a grid-sampled blur).
f = figure('Visible', 'off', 'Color', [1 1 1], 'Position', [100 100 400 400]);
[Xc, Yc] = meshgrid([0 1], [0 1]);
Zc = zeros(2, 2);
surf(Xc, Yc, Zc, tex, 'FaceColor', 'texturemap', 'EdgeColor', 'none');
view(2);
set(gca, 'YDir', 'normal');
drawnow();
F = getframe(f);
rr = double(F.cdata(:, :, 1));
bb = double(F.cdata(:, :, 3));
asserts.istrue(nnz(rr > 180 & bb < 100) > 500);
asserts.istrue(nnz(bb > 180 & rr < 100) > 500);
close(f);
%=============================================================================
