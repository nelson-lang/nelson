%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% randg draws gamma-distributed numbers (unit scale). The generator is a
% rejection method, so values are not reproducible against another tool, but
% within one RNG state they are deterministic and their statistics are known:
% a gamma(k) sample has mean k and variance k.
%=============================================================================
assert_isequal(nargin('randg'), -1);
%=============================================================================
% Sizes for every accepted signature.
assert_isequal(size(randg(1)), [1 1]);
assert_isequal(size(randg(1, 15)), [15 15]);
assert_isequal(size(randg(1, 1, 981)), [1 981]);
assert_isequal(size(randg(2, 60)), [60 60]);
assert_isequal(size(randg(2, 5, 5)), [5 5]);
assert_isequal(size(randg(1, [3 4])), [3 4]);
% A shape array with no size uses the size of A, element-wise.
assert_isequal(size(randg([1 2 3])), [1 3]);
assert_isequal(size(randg(ones(2, 4))), [2 4]);
%=============================================================================
% Every value is a nonnegative real.
v = randg(2, 50, 50);
assert_istrue(isreal(v));
assert_istrue(all(v(:) >= 0));
%=============================================================================
% Deterministic for a fixed RNG state.
rng(42);
a = randg(2, 20, 20);
rng(42);
b = randg(2, 20, 20);
assert_isequal(a, b);
%=============================================================================
% Class control.
assert_isequal(class(randg(1, 2, 2)), 'double');
assert_isequal(class(randg(1, 2, 2, 'single')), 'single');
%=============================================================================
% Distribution statistics: gamma(k) has mean k and variance k.
rng(7);
s1 = randg(1, 1, 200000);
assert_istrue(abs(mean(s1) - 1) < 0.05);
assert_istrue(abs(var(s1) - 1) < 0.1);
s3 = randg(3, 1, 200000);
assert_istrue(abs(mean(s3) - 3) < 0.1);
assert_istrue(abs(var(s3) - 3) < 0.2);
% A sub-one shape still works (boosted path).
sHalf = randg(0.5, 1, 200000);
assert_istrue(abs(mean(sHalf) - 0.5) < 0.05);
%=============================================================================
% A zero shape is deterministically zero.
assert_isequal(randg(0, 3, 3), zeros(3, 3));
%=============================================================================
% Invalid inputs are rejected.
assert_checkerror('randg(-1)', _('A must be a real array of nonnegative shape parameters.'));
assert_checkerror('randg(1, 2, ''bad'')', _('CLASSNAME must be ''single'' or ''double''.'));
assert_checkerror('randg([1 2], 3)', _('A must be a scalar when a size is supplied.'));
%=============================================================================
