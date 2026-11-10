%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nearest takes the upper grid point at an exact midpoint
V = single([1 2; 3 4]);
x = single([0 1]);
y = single([0 1]);
assert_isequal(interp2(x, y, V, single([0.4 0.5 0.6]), single([0 0 0]), 'nearest'), single([1 2 2]));
assert_isequal(interp2(x, y, V, single([0 0 0]), single([0.4 0.5 0.6]), 'nearest'), single([1 3 3]));
V3 = reshape(single(1:60), 4, 5, 3);
x3 = single([0 1 2 4 5]);
y3 = single([1 2 3 5]);
z3 = single([0 2 3]);
R = interp3(x3, y3, z3, V3, single([-1 0.5 2.2 4.9 5]), single([1 1.5 4 5 2]), single([0 1 2.5 3 3.5]), 'nearest');
assert_isequal(isnan(R), [true false false false true]);
assert_isequal(R(2:4), single([26 52 60]));
R = interpn(y3, x3, V3(:, :, 1), single([1 1.5 4 5 2]), single([-1 0.5 2.2 4.9 5]), 'nearest');
assert_istrue(isnan(R(1)));
assert_isequal(R(2:5), single([6 12 20 18]));
%=============================================================================
% a NaN query stays NaN with a fill value for the outside
R = interp2(x, y, V, single([NaN 2 0.5]), single([0.5 0.5 0.5]), 'linear', 0);
assert_istrue(isnan(R(1)));
assert_isequal(R(2:3), single([0 2.5]));
R = interp2(V, single([NaN 3 1.5]), single([1 1 1.5]), 'linear', 0);
assert_istrue(isnan(R(1)));
assert_isequal(R(2:3), single([0 2.5]));
R = interpn(y3, x3, V3(:, :, 1), single([NaN 1.5]), single([1 2]), 'linear', -1);
assert_istrue(isnan(R(1)));
assert_isapprox(R(2), single(9.5), 1e-6);
R = interp2(linspace(0, 1, 5), linspace(0, 1, 5), magic(5), [NaN 0.5], [0.5 NaN], 'linear', 7);
assert_istrue(all(isnan(R)));
%=============================================================================
% spline values keep single data in single
assert_isequal(class(spline(single([0 1 2.5 3]), single([0 1 0 1]), single(1.5))), 'single');
assert_isequal(class(spline([0 1 2.5 3], [0 1 0 1], 1.5)), 'double');
%=============================================================================
