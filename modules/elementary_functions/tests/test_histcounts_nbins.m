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
% Specified number of bins: edges are placed on a 'nice' grid that may extend
% beyond [min, max] while keeping exactly nbins bins (bin picker behaviour).
%=============================================================================
[~, e] = histcounts([1 3], 3);
asserts.isapprox(e, [0.6 1.4 2.2 3], 1e-9);
%=============================================================================
[~, e] = histcounts([1 3], 4);
asserts.isapprox(e, [1 1.5 2 2.5 3], 1e-9);
%=============================================================================
[~, e] = histcounts([0 10], 5);
asserts.isapprox(e, [0 2 4 6 8 10], 1e-9);
%=============================================================================
[~, e] = histcounts([1 100], 7);
asserts.isapprox(e, [0 15 30 45 60 75 90 105], 1e-9);
%=============================================================================
[~, e] = histcounts([1.1 3.3], 3);
asserts.isapprox(e, [0.7 1.6 2.5 3.4], 1e-9);
%=============================================================================
[~, e] = histcounts([0 1], 3);
asserts.isapprox(e, [0 0.4 0.8 1.2], 1e-9);
%=============================================================================
% Note the exact floating-point artifact on the third edge.
[~, e] = histcounts([-5 5], 4);
asserts.isapprox(e, [-6 -3.2 -0.399999999999999 2.4 5.2], 1e-9);
%=============================================================================
[~, e] = histcounts([1 10], 3);
asserts.isapprox(e, [0 4 8 12], 1e-9);
%=============================================================================
[~, e] = histcounts([0.1 0.9], 5);
asserts.isapprox(e, [0.1 0.26 0.42 0.58 0.74 0.9], 1e-9);
%=============================================================================
[~, e] = histcounts([1 1000], 3);
asserts.isapprox(e, [0 400 800 1200], 1e-9);
%=============================================================================
% A number of bins that coincides with uniform edges from min to max.
[N, e] = histcounts(1:10, 5);
asserts.isequal(N, [2 2 2 2 2]);
asserts.isapprox(e, [1 2.8 4.6 6.4 8.2 10], 1e-12);
%=============================================================================
% Boundary counts: values on an interior edge fall in the right bin, the last
% edge is closed on both sides.
[N, e] = histcounts([0 2 4 6 8 10], 5);
asserts.isapprox(e, [0 2 4 6 8 10], 1e-9);
asserts.isequal(N, [1 1 1 1 2]);
%=============================================================================
[N, e] = histcounts([1 3], 3);
asserts.isequal(N, [1 0 1]);
%=============================================================================
% Empty data with a requested number of bins.
[N, e] = histcounts([], 3);
asserts.isapprox(e, [0 1 2 3], 1e-9);
asserts.isequal(N, [0 0 0]);
%=============================================================================
% Constant data with a requested number of bins.
[~, e] = histcounts([5 5 5], 3);
asserts.isapprox(e, [4.5 4.83333333333333 5.16666666666667 5.5], 1e-12);
%=============================================================================
[~, e] = histcounts([0 0 0], 4);
asserts.isapprox(e, [-0.5 -0.25 0 0.25 0.5], 1e-9);
%=============================================================================
% Hard bin limits combined with a number of bins: uniform edges over the limits.
[~, e] = histcounts([1 50 100], 7, 'BinLimits', [0 100]);
asserts.isapprox(e, [0 (100 / 7) * (1:6) 100], 1e-10);
%=============================================================================
