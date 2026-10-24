%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
RGB = zeros(16, 16, 3);
[X, Y] = meshgrid(linspace(0, 1, 16), linspace(0, 1, 16));
RGB(:, :, 1) = X;
RGB(:, :, 2) = Y;
RGB(:, :, 3) = 1 - X;
G1 = im2gray(RGB);
G2 = rgb2gray(RGB);
HSV = rgb2hsv(RGB);
YCBCR = rgb2ycbcr(RGB);
RGB2 = ycbcr2rgb(YCBCR);
asserts.isapprox(G1, G2, 1e-15);
asserts.isequal(size(HSV), [16 16 3]);
asserts.isequal(size(YCBCR), [16 16 3]);
asserts.isapprox(RGB2, RGB, 2e-3);
