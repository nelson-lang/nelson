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
asserts.isequal(nargin('histcounts2'), -1);
asserts.isequal(nargout('histcounts2'), -1);
%=============================================================================
% Explicit edges on a 2-D grid; last bin closed on both ends in each dimension.
N = histcounts2([1 2 3], [1 2 3], [0 2 4], [0 2 4]);
asserts.isequal(N, [1 0; 0 2]);
%=============================================================================
% n(i,j) counts pairs with xedges(i) <= x < xedges(i+1) (rows) and
% yedges(j) <= y < yedges(j+1) (columns).
N = histcounts2(5, 1, [0 2 6], [0 2 6]);
asserts.isequal(N, [0 0; 1 0]);
%=============================================================================
% Last bin is closed on both ends.
N = histcounts2([0 2 4], [0 2 4], [0 2 4], [0 2 4]);
asserts.isequal(N, [1 0; 0 2]);
%=============================================================================
% NaN in either coordinate drops the pair.
N = histcounts2([1 NaN 3], [1 2 NaN], [0 2 4], [0 2 4]);
asserts.isequal(N, [1 0; 0 0]);
%=============================================================================
% Scalar number of bins: nice edges shared with histcounts, returned as
% second and third outputs.
[N, xe, ye] = histcounts2([1 5 10 3 7], [2 4 6 8 1], 3);
asserts.isequal(N, [1 0 1; 1 1 0; 0 0 1]);
asserts.isapprox(xe, [0 4 8 12], 1e-10);
asserts.isapprox(ye, [0 3 6 9], 1e-10);
%=============================================================================
% The number-of-bins edges match the 1-D histcounts bin picker exactly.
[~, hxe] = histcounts([1 5 10 3 7], 3);
[~, hye] = histcounts([2 4 6 8 1], 3);
[~, xe, ye] = histcounts2([1 5 10 3 7], [2 4 6 8 1], 3);
asserts.isequal(xe, hxe);
asserts.isequal(ye, hye);
%=============================================================================
% A two-element vector requests a different number of bins per dimension.
[N, xe, ye] = histcounts2([1 5 10 3 7], [2 4 6 8 1], [2 3]);
asserts.isequal(N, [1 0 1; 1 1 1]);
asserts.isapprox(xe, [0 5 10], 1e-10);
asserts.isapprox(ye, [0 3 6 9], 1e-10);
%=============================================================================
% Automatic bin selection uses per-dimension histcounts edges.
[N, xe, ye] = histcounts2([1 2], [3 4]);
asserts.isapprox(xe, [0.5 1.5 2.5], 1e-10);
asserts.isapprox(ye, [2.5 3.5 4.5], 1e-10);
%=============================================================================
% Normalizations on a uniform grid.
asserts.isapprox(histcounts2([1 2 3], [1 2 3], [0 2 4], [0 2 4], ...
  'Normalization', 'probability'), [1 / 3 0; 0 2 / 3], 1e-12);
asserts.isapprox(histcounts2([1 2 3], [1 2 3], [0 2 4], [0 2 4], ...
  'Normalization', 'pdf'), [1 / 12 0; 0 1 / 6], 1e-12);
asserts.isapprox(histcounts2([1 2 3], [1 2 3], [0 2 4], [0 2 4], ...
  'Normalization', 'countdensity'), [0.25 0; 0 0.5], 1e-12);
asserts.isequal(histcounts2([1 2 3], [1 2 3], [0 2 4], [0 2 4], ...
  'Normalization', 'cumcount'), [1 1; 1 3]);
%=============================================================================
% Normalizations on a non-uniform grid use per-cell bin areas.
x = [1 1 3 3 5]; y = [1 3 1 3 5]; xe = [0 2 6]; ye = [0 2 6];
asserts.isequal(histcounts2(x, y, xe, ye), [1 1; 1 2]);
asserts.isapprox(histcounts2(x, y, xe, ye, 'Normalization', 'pdf'), ...
  [0.05 0.025; 0.025 0.025], 1e-12);
asserts.isapprox(histcounts2(x, y, xe, ye, 'Normalization', 'countdensity'), ...
  [0.25 0.125; 0.125 0.125], 1e-12);
asserts.isapprox(histcounts2(x, y, xe, ye, 'Normalization', 'cdf'), ...
  [0.2 0.4; 0.4 1.0], 1e-12);
%=============================================================================
% Matrix inputs are treated element-wise as pairs.
N = histcounts2([1 2; 3 4], [1 2; 3 4], [0 2 4], [0 2 4]);
asserts.isequal(N, [1 0; 0 3]);
%=============================================================================
asserts.checkerror('histcounts2([1 2 3], [1 2])', ...
  _('X and Y must have the same number of elements.'));
%=============================================================================
asserts.checkerror('histcounts2([1 2 3], [1 2 3], [4 2 0], [0 2 4])', ...
  _('Bin edges must be a monotonically increasing vector.'));
%=============================================================================
