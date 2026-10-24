%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sparse partial permutation randperm(n, k) with 4*k < n uses selection
% sampling and must reproduce the expected stream bit-for-bit.
%=============================================================================
rng(1, 'twister');
asserts.isequal(randperm(100, 5), [42 72 1 30 15]);
%=============================================================================
rng(1, 'twister');
asserts.isequal(randperm(1000, 10), [418 720 1 302 147 92 186 344 394 534]);
%=============================================================================
rng(1, 'twister');
asserts.isequal(randperm(20, 3), [9 14 1]);
%=============================================================================
rng(1, 'twister');
asserts.isequal(randperm(50, 8), [21 36 1 15 7 5 9 47]);
%=============================================================================
rng(42, 'twister');
asserts.isequal(randperm(100, 5), [38 95 72 59 15]);
%=============================================================================
rng(7, 'twister');
asserts.isequal(randperm(30, 5), [3 23 13 20 26]);
%=============================================================================
rng(1, 'twister');
asserts.isequal(randperm(40, 1), 17);
%=============================================================================
% Full randperm(n) is unchanged (still matches expected behavior).
rng(1, 'twister');
asserts.isequal(randperm(10), [3 6 5 7 4 8 9 1 10 2]);
%=============================================================================
% Selection is stable under re-seeding.
rng(2024, 'twister');
a = randperm(500, 20);
rng(2024, 'twister');
b = randperm(500, 20);
asserts.isequal(a, b);
asserts.isequal(numel(unique(a)), 20);
%=============================================================================
