%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('cart2pol'), -1);
assert_isequal(nargout('cart2pol'), -1);
%=============================================================================
x = [5 3.5355 0 -10];
y = [0 3.5355 10 0];
[theta, rho] = cart2pol(x, y);
theta_REF = [0    0.7854    1.5708    3.1416];
rho_REF = [5.0000    5.0000   10.0000   10.0000];
assert_isapprox(theta, theta_REF, 1e-4);
assert_isapprox(rho, rho_REF, 1e-4);
%=============================================================================
x = [1 2.1213 0 -5];
y = [0 2.1213 4 0];
z = [7 8 9 10];
[theta, rho, el] = cart2pol(x, y, z);
theta_REF = [0    0.7854    1.5708    3.1416];
rho_REF = [1.0000    3.0000    4.0000    5.0000];
el_REF = [7     8     9    10];
assert_isapprox(theta, theta_REF, 1e-4);
assert_isapprox(rho, rho_REF, 1e-4);
assert_isapprox(el, el_REF, 1e-4);
%=============================================================================
