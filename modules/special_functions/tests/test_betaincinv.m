%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isapprox(betaincinv([0.1 0.5 0.9], 2, 3), [0.142559316710031 0.385727568132389 0.679539416278182], 1e-13);
assert_isapprox(betaincinv([0.1 0.5 0.9], 2, 3, 'upper'), [0.679539416278182 0.385727568132389 0.142559316710031], 1e-13);
assert_isequal(betaincinv([0 1], 2, 3), [0 1]);
assert_isapprox(betaincinv([0.3 0.6], 0.1, 0.2), [0.000259700094900934 0.222221291712662], 1e-11);
assert_isapprox(betaincinv(0.4, 500, 300), 0.6207601581588, 1e-12);
assert_isequal(class(betaincinv(single(0.5), 2, 3)), 'single');
z = [0.5 1 2 10 0.2];
w = [0.5 3 2 1 5];
y = [0.01 0.3 0.6 0.9 0.999];
assert_isapprox(betainc(betaincinv(y, z, w), z, w), y, 1e-12);
x = betaincinv(1e-200, 2, 3, 'upper');
assert_isapprox(betainc(x, 2, 3, 'upper'), 1e-200, 1e-10);
assert_checkerror('betaincinv(-0.1, 2, 3)', _('Y must be in the interval [0, 1].'));
assert_checkerror('betaincinv(0.5, -2, 3)', _('Z and W must be nonnegative.'));
%=============================================================================
