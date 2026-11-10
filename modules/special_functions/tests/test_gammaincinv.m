%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isapprox(gammaincinv([0.1 0.5 0.9], 2), [0.531811608389612 1.67834699001666 3.88972016986743], 1e-13);
assert_isapprox(gammaincinv([0.1 0.5 0.9], 2, 'upper'), [3.88972016986743 1.67834699001666 0.531811608389612], 1e-13);
assert_isequal(gammaincinv([0 1], 3), [0 Inf]);
assert_isequal(gammaincinv([0 1], 3, 'upper'), [Inf 0]);
assert_isapprox(gammaincinv([0.2 0.7], 0.01), [7.1758381098579e-71 1.83095245638084e-16], 1e-12);
assert_isapprox(gammaincinv([0.2 0.7], 1000), [973.293038491621 1016.33825137867], 1e-13);
assert_isequal(gammaincinv(0.5, 0), 0);
assert_isequal(class(gammaincinv(single(0.5), 2)), 'single');
assert_istrue(isnan(gammaincinv(NaN, 2)));
x = gammaincinv(1e-300, 5, 'upper');
assert_isapprox(gammainc(x, 5, 'upper'), 1e-300, 1e-12);
a = [0.5 1 3 10 50];
p = [0.01 0.3 0.6 0.9 0.999];
assert_isapprox(gammainc(gammaincinv(p, a), a), p, 1e-13);
assert_checkerror('gammaincinv(1.5, 2)', _('Y must be in the interval [0, 1].'));
assert_checkerror('gammaincinv([0.5; 0.6], [1 2])', _('Inputs must have the same size, or be scalars.'));
assert_checkerror('gammaincinv(0.5, -1)', _('A must be nonnegative.'));
assert_checkerror('gammaincinv(0.5, 2, ''foo'')', _('Tail must be ''lower'' or ''upper''.'));
%=============================================================================
