%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 3-->
% <--ADV-CLI MODE-->
%=============================================================================
% A matrix handed to hist() is a set of samples, one per column: each column is
% counted on its own and gets its own bar series. The bins themselves are read
% on the whole sample, so every column is counted against the same bins.
%
% The expected values below are frozen regression values.
%=============================================================================
% Ten bins by default, one column of counts per column of the sample.
[n, x] = hist(magic(4));
assert_isequal(size(n), [10 4]);
assert_isequal(n, [...
   0 1 0 1; ...
   1 0 1 0; ...
   1 0 0 0; ...
   0 1 1 0; ...
   0 0 0 1; ...
   1 0 1 0; ...
   0 1 0 0; ...
   0 0 0 2; ...
   0 1 0 0; ...
   1 0 1 0]);
assert_isequal(size(x), [10 1]);
assert_isapprox(x, [1.75; 3.25; 4.75; 6.25; 7.75; 9.25; 10.75; 12.25; 13.75; 15.25], 1e-10);
%=============================================================================
% One output only: same counts.
n = hist(magic(4));
assert_isequal(size(n), [10 4]);
assert_isequal(sum(n), [4 4 4 4]);
%=============================================================================
% A bin count asked for explicitly.
[n, x] = hist(magic(4), 3);
assert_isequal(n, [2 1 2 1; 1 2 1 1; 1 1 1 2]);
assert_isequal(size(x), [3 1]);
assert_isapprox(x, [3.5; 8.5; 13.5], 1e-10);
%=============================================================================
[n, x] = hist(magic(4), 4);
assert_isequal(n, ones(4, 4));
assert_isequal(size(x), [4 1]);
assert_isapprox(x, [2.875; 6.625; 10.375; 14.125], 1e-10);
%=============================================================================
% The bins are read on the WHOLE sample, never column by column: the first
% column of this matrix holds 1 2 3 and lands wholly in the first bin of a
% 1..300 span. Per-column bins would have spread it over the ten bins.
[n, x] = hist([1 100; 2 200; 3 300]);
assert_isequal(size(x), [10 1]);
assert_isapprox(x, [15.95; 45.85; 75.75; 105.65; 135.55; 165.45; 195.35; 225.25; 255.15; 285.05], 1e-10);
assert_isequal(n, [...
   3 0; ...
   0 0; ...
   0 0; ...
   0 1; ...
   0 0; ...
   0 0; ...
   0 1; ...
   0 0; ...
   0 0; ...
   0 1]);
%=============================================================================
% Centres given as a row: the counts stay one column per column of the sample.
[n, x] = hist(magic(4), [2 8 14]);
assert_isequal(n, [2 1 1 1; 1 2 2 1; 1 1 1 2]);
assert_isequal(size(x), [3 1]);
assert_isequal(x, [2; 8; 14]);
%=============================================================================
% Same centres given as a column: same counts, centres handed back transposed.
[n, x] = hist(magic(4), [2; 8; 14]);
assert_isequal(n, [2 1 1 1; 1 2 2 1; 1 1 1 2]);
assert_isequal(size(x), [1 3]);
assert_isequal(x, [2 8 14]);
%=============================================================================
% A NaN is left out of the count of ITS column and of no other one.
[n, x] = hist([1 NaN 3; 4 5 6; 7 8 9], 3);
assert_isequal(n, [1 0 1; 1 1 1; 1 1 1]);
assert_isequal(sum(n), [3 2 3]);
assert_isapprox(x, [2.3333333333333330; 5; 7.6666666666666661], 1e-10);
%=============================================================================
% A column that is nothing but NaN is counted as empty, the other one stands.
[n, x] = hist([1 NaN; 2 NaN; 3 NaN], 3);
assert_isequal(n, [1 0; 1 0; 1 0]);
assert_isapprox(x, [1.3333333333333333; 2; 2.6666666666666665], 1e-10);
%=============================================================================
% A constant column piles up in the single bin holding its value.
[n, x] = hist([1 5; 2 5; 3 5], 4);
assert_isequal(n, [2 0; 1 0; 0 0; 0 3]);
assert_isapprox(x, [1.5; 2.5; 3.5; 4.5], 1e-10);
%=============================================================================
% Enough bins to take the vectorised counting path (30 bins and over). Here the
% values 4 and 7 sit exactly on a cut point and must be counted in the bin BELOW
% it (rows 8 and 16, not 9 and 17), which the sort inside that path only gets
% right if it is not left to pick the order of a tie on its own.
[n, x] = hist(magic(4), 40);
assert_isequal(size(n), [40 4]);
assert_isequal(size(x), [40 1]);
assert_isequal(sum(n), [4 4 4 4]);
filled = find(any(n, 2));
assert_isequal(filled(:)', [1 3 6 8 11 14 16 19 22 24 27 30 32 35 38 40]);
assert_isequal(n(filled, :), [...
   0 0 0 1; ...
   0 1 0 0; ...
   0 0 1 0; ...
   1 0 0 0; ...
   1 0 0 0; ...
   0 0 1 0; ...
   0 1 0 0; ...
   0 0 0 1; ...
   1 0 0 0; ...
   0 0 1 0; ...
   0 1 0 0; ...
   0 0 0 1; ...
   0 0 0 1; ...
   0 1 0 0; ...
   0 0 1 0; ...
   1 0 0 0]);
%=============================================================================
% Only the trailing dimensions are folded onto columns: a 2x2x2 array is
% counted as four samples of two values.
y = reshape([1 3 2 4 5 7 6 8], [2 2 2]);
[n, x] = hist(y);
assert_isequal(size(n), [10 4]);
assert_isequal(n, [...
   1 0 0 0; ...
   0 1 0 0; ...
   1 0 0 0; ...
   0 0 0 0; ...
   0 1 0 0; ...
   0 0 1 0; ...
   0 0 0 0; ...
   0 0 0 1; ...
   0 0 1 0; ...
   0 0 0 1]);
assert_isequal(size(x), [10 1]);
%=============================================================================
% A complex sample is counted on its real part, column by column.
[n, x] = hist(complex(magic(4), magic(4)));
[nReal, xReal] = hist(magic(4));
assert_isequal(n, nReal);
assert_isequal(x, xReal);
%=============================================================================
% Drawing: one bar series per column of the sample.
f = figure('Visible', 'off');
ax = axes('Parent', f);
hist(ax, magic(4));
assert_isequal(numel(ax.Children), 4);
close(f);
%=============================================================================
% A vector is a single sample and draws a single series.
f = figure('Visible', 'off');
ax = axes('Parent', f);
hist(ax, (1:10)');
assert_isequal(numel(ax.Children), 1);
close(f);
%=============================================================================
% A vector keeps its own contract: counts come back as a row.
[n, x] = hist(1:10);
assert_isequal(size(n), [1 10]);
assert_isequal(size(x), [1 10]);
[n, x] = hist((1:10)');
assert_isequal(size(n), [1 10]);
assert_isequal(size(x), [1 10]);
[n, x] = hist(1:10, [2 5 8]);
assert_isequal(size(n), [1 3]);
assert_isequal(x, [2 5 8]);
%=============================================================================
% Centres given as a column to a vector sample come back as a column.
[n, x] = hist(1:10, [2; 5; 8]);
assert_isequal(size(n), [1 3]);
assert_isequal(size(x), [3 1]);
assert_isequal(x, [2; 5; 8]);
%=============================================================================
% A single column of a matrix is a vector and is counted as one.
m = magic(4);
[n, x] = hist(m(:, 1), 3);
assert_isequal(size(n), [1 3]);
assert_isequal(size(x), [1 3]);
%=============================================================================
