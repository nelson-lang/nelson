%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% hist() answers the same counts whichever module supplies it: the graphics
% module shadows the statistics one as soon as it is loaded, so the two have to
% agree bin for bin. The cases below are the ones where they used to differ.
%=============================================================================
% A sample is read the same way whichever way round it is written.
[n, c] = hist([1 2 2 3 3 3 4 9], 3);
assert_isequal(n, [6 1 1]);
assert_isapprox(c, [2.333333333333333 5 7.666666666666667], 1e-10);
assert_isequal(size(n), [1 3]);
assert_isequal(size(c), [1 3]);
%=============================================================================
[n, c] = hist([1 2 2 3 3 3 4 9]', 3);
assert_isequal(n, [6 1 1]);
assert_isapprox(c, [2.333333333333333 5 7.666666666666667], 1e-10);
%=============================================================================
% A sample with no spread at all still gets its bins laid out around it: they
% are one unit wide and the value sits in the middle one.
[n, c] = hist([5 5 5 5], 4);
assert_isequal(n, [0 0 4 0]);
assert_isequal(c, [3 4 5 6]);
%=============================================================================
[n, c] = hist(7, 3);
assert_isequal(n, [0 1 0]);
assert_isequal(c, [6 7 8]);
%=============================================================================
% An empty sample counts nothing rather than failing. It is not a vector, so it
% is read as a set of samples and answers a column.
[n, c] = hist([], 3);
assert_isequal(n, [0; 0; 0]);
assert_isapprox(c, [1 / 6; 1 / 2; 5 / 6], 1e-10);
%=============================================================================
% A bin count has to be a positive number.
positiveMessage = getString(message('Nelson:validators:mustBePositive', 'Second argument'));
assert_checkerror('hist([1 2 3], 0)', positiveMessage, 'Nelson:validators:mustBePositive');
assert_checkerror('hist([1 2 3], -2)', positiveMessage, 'Nelson:validators:mustBePositive');
%=============================================================================
% Only the real part of a sample is counted.
[nc, cc] = hist([1 + 2i 3 + 4i 5], 3);
[nr, cr] = hist([1 3 5], 3);
assert_isequal(nc, nr);
assert_isequal(cc, cr);
%=============================================================================
% An integer sample is counted on its values, not on what its type can hold.
[ni, ci] = hist(int32([1 2 3 4 5]), 3);
[nd, cd] = hist([1 2 3 4 5], 3);
assert_isequal(ni, nd);
assert_isapprox(ci, cd, 1e-10);
%=============================================================================
[ni, ci] = hist(uint8([10 40 70 20 50 80]), 3);
[nd, cd] = hist([10 40 70 20 50 80], 3);
assert_isequal(ni, nd);
assert_isapprox(ci, cd, 1e-10);
%=============================================================================
% A matrix is a set of samples, one per column, counted against shared bins.
[n, c] = hist([1 2; 3 4; 5 6], 3);
assert_isequal(n, [1 1; 1 1; 1 1]);
assert_isequal(size(n), [3 2]);
assert_isapprox(c, [1.833333333333333; 3.5; 5.166666666666667], 1e-10);
assert_isequal(size(c), [3 1]);
%=============================================================================
% Given centres are handed back the way they were given.
[n, c] = hist([1 2 2 3 3 3 4 9], [1 3 5 7 9]);
assert_isequal(n, [3 4 0 0 1]);
assert_isequal(c, [1 3 5 7 9]);
assert_isequal(size(c), [1 5]);
%=============================================================================
[n, c] = hist([1 2 2 3 3 3 4 9], [1 3 5 7 9]');
assert_isequal(n, [3 4 0 0 1]);
assert_isequal(c, [1; 3; 5; 7; 9]);
assert_isequal(size(c), [5 1]);
%=============================================================================
% Centres are read in increasing order whichever order they were handed in.
[nu, cu] = hist([1 2 3 4 5], [5 1 3]);
[ns, cs] = hist([1 2 3 4 5], [1 3 5]);
assert_isequal(nu, ns);
assert_isequal(cu, cs);
%=============================================================================
% A sample that is not a number is counted in no bin at all.
[n, c] = hist([1 2 NaN 3 3], 3);
assert_isequal(n, [1 1 2]);
%=============================================================================
