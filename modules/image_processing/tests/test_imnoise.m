%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng(0);
%=============================================================================
% gaussian: zero-mean noise added to a mid-gray image keeps the mean near 0.5,
% stays in [0, 1], and preserves class.
g = imnoise(0.5 * ones(300), 'gaussian', 0, 0.01);
assert_isequal(class(g), 'double');
assert_istrue(all(g(:) >= 0 & g(:) <= 1));
assert_istrue(abs(mean(g(:)) - 0.5) < 0.02);
%=============================================================================
% salt & pepper at density d corrupts ~d of the pixels to 0 or 1.
sp = imnoise(0.5 * ones(400), 'salt & pepper', 0.1);
frac = mean(sp(:) == 0 | sp(:) == 1);
assert_istrue(abs(frac - 0.1) < 0.02);
assert_istrue(all(sp(:) == 0 | sp(:) == 1 | sp(:) == 0.5));
%=============================================================================
% speckle is zero-mean multiplicative noise: the mean is preserved.
sk = imnoise(0.5 * ones(300), 'speckle', 0.04);
assert_istrue(abs(mean(sk(:)) - 0.5) < 0.02);
%=============================================================================
% class is preserved for integer images.
u = imnoise(uint8(128 * ones(16)), 'gaussian', 0, 0.01);
assert_isequal(class(u), 'uint8');
%=============================================================================
% default type is gaussian.
d = imnoise(0.5 * ones(8));
assert_isequal(class(d), 'double');
%=============================================================================
% unknown noise type errors.
assert_checkerror('imnoise(zeros(4), ''bogus'')', 'Unknown noise type: bogus.');
%=============================================================================
