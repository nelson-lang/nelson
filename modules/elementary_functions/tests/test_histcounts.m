%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('histcounts'), -1);
asserts.isequal(nargout('histcounts'), -1);
%=============================================================================
% Explicit edges, last bin closed on the right.
N = histcounts([1 2 2 3 3 3], [0.5 1.5 2.5 3.5]);
asserts.isequal(N, [1 2 3]);
%=============================================================================
% Number of bins : uniform edges from min to max.
[N, e] = histcounts(1:10, 5);
asserts.isequal(N, [2 2 2 2 2]);
asserts.isapprox(e, [1 2.8 4.6 6.4 8.2 10], 1e-10);
%=============================================================================
% Automatic bin selection for integer data.
[N, e] = histcounts([1 2 3]);
asserts.isequal(N, [1 1 1]);
asserts.isapprox(e, [0.5 1.5 2.5 3.5], 1e-10);
%=============================================================================
% Third output : bin index for each element (input shape preserved).
[N, e, b] = histcounts([1 2 2 3 3 3], [0.5 1.5 2.5 3.5]);
asserts.isequal(b, [1 2 2 3 3 3]);
%=============================================================================
% Values outside the edges are not counted (bin index 0).
[N, e, b] = histcounts([0 1 2 5], [0.5 1.5 2.5]);
asserts.isequal(N, [1 1]);
asserts.isequal(b, [0 1 2 0]);
%=============================================================================
% NaN values are ignored.
N = histcounts([1 NaN 2 3], [0.5 1.5 2.5 3.5]);
asserts.isequal(N, [1 1 1]);
%=============================================================================
% BinMethod and Normalization options.
asserts.isequal(histcounts([1 2 2 3 3 3], 'BinMethod', 'integers'), [1 2 3]);
asserts.isapprox(histcounts([1 1 2 2], 'Normalization', 'probability'), [0.5 0.5], 1e-12);
asserts.isapprox(histcounts([1 2 3 4], [0 2 4], 'Normalization', 'pdf'), [0.125 0.375], 1e-12);
asserts.isapprox(histcounts([1 2 3 4], 4, 'Normalization', 'cdf'), [0.25 0.5 0.75 1], 1e-12);
asserts.isequal(histcounts([1 2 3 4 5], [0 2 4 6], 'Normalization', 'cumcount'), [1 3 5]);
asserts.isequal(histcounts([1 2 3 4 5], 'NumBins', 5), [1 1 1 1 1]);
asserts.isequal(histcounts([1 2 3 4 5 6], 'NumBins', 3, 'BinLimits', [0 6]), [1 2 3]);
%=============================================================================
