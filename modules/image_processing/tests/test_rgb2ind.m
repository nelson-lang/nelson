%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
%=============================================================================
Rr = uint8([10 10 240 240; 10 30 240 15; 250 30 120 0; 250 250 120 0]);
Gg = uint8([20 20 10 10; 20 220 10 15; 250 220 120 0; 250 250 120 0]);
Bb = uint8([200 200 10 10; 200 40 10 15; 250 40 120 0; 250 10 120 0]);
RGB = cat(3, Rr, Gg, Bb);
%=============================================================================
% Minimum-variance quantization with fewer colors than requested is lossless.
[X, map] = rgb2ind(RGB, 16, 'nodither');
asserts.isequal(class(X), 'uint8');
asserts.isequal(size(map, 2), 3);
asserts.isequal(size(map, 1), 8);
asserts.isequal(max(X(:)), uint8(7));
asserts.isapprox(ind2rgb(X, map), im2double(RGB), 1e-12);
%=============================================================================
% Requesting fewer colors than present builds a colormap with exactly N entries.
[X4, map4] = rgb2ind(RGB, 4, 'nodither');
asserts.isequal(size(map4, 1), 4);
asserts.istrue(max(X4(:)) <= uint8(3));
asserts.istrue(all(map4(:) >= 0) && all(map4(:) <= 1));
%=============================================================================
% Mapping onto a supplied palette uses the nearest color.
pal = [0 0 0; 1 1 1; 1 0 0; 0 0 1];
Xp = rgb2ind(RGB, pal, 'nodither');
REF = uint8([3 3 2 2; 3 0 2 0; 1 0 0 0; 1 1 0 0]);
asserts.isequal(class(Xp), 'uint8');
asserts.isequal(Xp, REF);
%=============================================================================
% A colormap with more than 256 entries returns uint16 indices.
palBig = repmat(linspace(0, 1, 300)', 1, 3);
XpBig = rgb2ind(RGB, palBig, 'nodither');
asserts.isequal(class(XpBig), 'uint16');
%=============================================================================
% Uniform quantization from a tolerance in (0, 1).
[Xt, mapt] = rgb2ind(im2double(RGB), 0.25, 'nodither');
REF_MAP = [0 0 0; 1 0 0; 1 1 0; 0 0.75 0.25; 0.5 0.5 0.5; 0 0 0.75; 1 1 1];
asserts.isequal(size(mapt, 1), 7);
asserts.isapprox(mapt, REF_MAP, 1e-12);
% every colormap entry lies on the uniform grid (multiples of 0.25).
asserts.isapprox(mapt * 4, round(mapt * 4), 1e-12);
%=============================================================================
% Dithering is the default and yields valid indices into the colormap.
[Xd, mapd] = rgb2ind(RGB, 4);
asserts.isequal(size(mapd, 1), 4);
asserts.istrue(max(Xd(:)) <= uint8(3));
%=============================================================================
% Single color request.
[X1, map1] = rgb2ind(RGB, 1, 'nodither');
asserts.isequal(size(map1), [1 3]);
asserts.isequal(max(X1(:)), uint8(0));
%=============================================================================
% Empty image returns an empty index array.
E = zeros(0, 0, 3);
[Xe, mape] = rgb2ind(E, 4);
asserts.isequal(size(Xe), [0 0]);
asserts.isequal(class(Xe), 'uint8');
asserts.isequal(size(mape), [1 3]);
%=============================================================================
% Double input is treated as being in the range [0, 1].
[Xdbl, mapdbl] = rgb2ind(im2double(RGB), 4, 'nodither');
asserts.isequal(size(mapdbl, 1), 4);
asserts.isequal(Xdbl, X4);
%=============================================================================
asserts.checkerror('rgb2ind(uint8([1 2; 3 4]), 4)', ...
  sprintf(_('%s must be an M-by-N-by-3 array.'), 'RGB'));
asserts.checkerror('rgb2ind(cat(3, [1 0], [0 1], [0 0]), 4, ''foo'')', ...
  _('DITHER_OPTION must be ''dither'' or ''nodither''.'));
asserts.checkerror('rgb2ind(cat(3, [1 0], [0 1], [0 0]), -2)', ...
  _('N must be an integer >= 1 or a tolerance in the interval (0, 1).'));
asserts.checkerror('rgb2ind(cat(3, [1 0], [0 1], [0 0]), [1 2; 3 4])', ...
  _('Second argument must be a scalar number of colors, a scalar tolerance, or an M-by-3 colormap.'));
%=============================================================================
