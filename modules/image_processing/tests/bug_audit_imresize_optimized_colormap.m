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
% <--ADV-CLI MODE-->
%=============================================================================
% Regression test: with 'Colormap','optimized', imresize prunes unused colors
% from the colormap but must also remap the pixel indices so the reconstructed
% colors are unchanged. Previously the indices were left pointing at the old
% colormap rows, producing wrong (or out-of-range) color lookups.
%=============================================================================
% 4x4 uint8 indexed image using 0-based indices {0, 2, 4, 6}, so rows
% (0-based) 1, 3, 5 and 7 of the 8-row colormap are unused.
X = uint8([0 2 4 6; 6 4 2 0; 0 0 2 2; 4 4 6 6]);
map = [(0:7)', (7:-1:0)', (mod((0:7) + 3, 8))'] / 7; % 8x3, distinct rows
%=============================================================================
% Resize at identical size (nearest neighbor identity), pruning the colormap.
[Y, mapOpt] = imresize(X, map, [4 4], 'Colormap', 'optimized');
% Only 4 colors are actually used, so the optimized colormap has 4 rows.
assert(size(mapOpt, 1) == 4);
assert(size(mapOpt, 2) == 3);
% The reconstructed RGB image must be identical before and after optimization.
rgbBefore = ind2rgb(X, map);
rgbAfter = ind2rgb(Y, mapOpt);
asserts.isapprox(rgbBefore, rgbAfter, 1e-12);
%=============================================================================
% 'original' mode must keep both the indices and the full colormap unchanged.
[Yorig, mapOrig] = imresize(X, map, [4 4], 'Colormap', 'original');
asserts.isequal(Yorig, X);
asserts.isapprox(mapOrig, map, 1e-12);
%=============================================================================
