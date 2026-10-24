%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Modified Akima interpolation ('makima'). Expected values were produced with
% an independent numerical engine implementing the same modified Akima scheme.
tol = 1e-11;
%=============================================================================
% Basic query on a uniform grid (regression: used to return 17.5).
x = [1 2 3 4 5]; y = [10 20 15 25 30];
R = interp1(x, y, 2.5, 'makima');
REF = 17.0625;
assert_isapprox(R, REF, tol);
%=============================================================================
% Vector query including exactly the nodes.
x = [1 2 3 4 5]; y = [10 20 15 25 30];
xq = [1 1.5 2 2.5 3 3.5 4 4.5 5];
R = interp1(x, y, xq, 'makima');
REF = [10 16.723557692307693 20 17.0625 15 19.65625 25 28.078125 30];
assert_isapprox(R, REF, tol);
% Interpolation must pass through the sample points.
assert_isapprox(interp1(x, y, x, 'makima'), y, tol);
%=============================================================================
% Non-uniform sample points.
x = [0 0.5 1.5 3 3.2 6]; y = [2 -1 4 5 5.5 0];
xq = [0.1 0.7 2.0 3.1 4.0 5.9];
R = interp1(x, y, xq, 'makima');
REF = [1.0866057428214733 -0.23322980352771339 4.4312092938668792 ...
  5.2633455712577826 5.428392894861247 0.31745783673406425];
assert_isapprox(R, REF, tol);
%=============================================================================
% Default extrapolation: 'makima' extrapolates (returns finite values).
x = [1 2 3 4 5]; y = [10 20 15 25 30];
xq = [-1 0 6 7.5];
R = interp1(x, y, xq, 'makima');
REF = [34.307692307692378 3.3461538461538396 25.5 -20.390625];
assert_isapprox(R, REF, tol);
assert_isfalse(any(isnan(R)));
%=============================================================================
% Explicit 'extrap'.
x = [1 2 3 4 5]; y = [10 20 15 25 30];
xq = [-2 0.5 5.5 8];
R = interp1(x, y, xq, 'makima', 'extrap');
REF = [137.15384615384619 4.1129807692307701 29.546875 -51];
assert_isapprox(R, REF, tol);
%=============================================================================
% Constant extrapolation value.
x = [1 2 3 4 5]; y = [10 20 15 25 30];
xq = [-2 2.5 8];
R = interp1(x, y, xq, 'makima', 99);
REF = [99 17.0625 99];
assert_isapprox(R, REF, tol);
%=============================================================================
% Matrix sample values, interpolated column-wise.
x = [1 2 3 4 5];
Y = [10 20 15 25 30; 0 5 3 -2 7].';
xq = [1.5 2.5 3.5];
R = interp1(x, Y, xq, 'makima');
REF = [16.723557692307693 3.3563966275659824; ...
  17.0625 4.3886247680890538; ...
  19.65625 0.43325412356723514];
assert_isequal(size(R), [3 2]);
assert_isapprox(R, REF, tol);
%=============================================================================
% Only two sample points reduces to linear.
x = [1 3]; y = [4 10];
xq = [1 2 3 0 4];
R = interp1(x, y, xq, 'makima');
REF = [4 7 10 1 13];
assert_isapprox(R, REF, tol);
%=============================================================================
% Three sample points.
x = [0 1 2]; y = [1 3 2];
xq = [0.25 0.5 1.5 1.75 -0.5 2.5];
R = interp1(x, y, xq, 'makima');
REF = [1.6897536057692308 2.3040865384615383 2.78125 2.4375 ...
  -0.29927884615384581 1.03125];
assert_isapprox(R, REF, tol);
%=============================================================================
% Decreasing sample points give the same interpolant.
x = [5 4 3 2 1]; y = [30 25 15 20 10];
R = interp1(x, y, [2.5 4.5 1.5], 'makima');
REF = [17.0625 28.078125 16.723557692307693];
assert_isapprox(R, REF, tol);
%=============================================================================
% Non-uniform grid with negative coordinates.
x = [-3 -1 0 2 5 9]; y = [7 -2 0 3 -4 8];
xq = [-2.5 -0.5 1.0 3.5 7.7];
R = interp1(x, y, xq, 'makima');
REF = [3.7265998803827749 -1.0704308712121213 1.8166392543859651 ...
  -0.028429680759275211 2.1892034277198231];
assert_isapprox(R, REF, tol);
%=============================================================================
% NaN queries produce NaN.
x = [1 2 3 4 5]; y = [10 20 15 25 30];
R = interp1(x, y, [2.5 NaN 3.5], 'makima');
assert_isapprox(R([1 3]), [17.0625 19.65625], tol);
assert_istrue(isnan(R(2)));
%=============================================================================
% Other methods are unchanged by the makima path.
x = [1 2 3 4 5]; y = [10 20 15 25 30]; xq = [1.5 2.5 3.5 4.5];
assert_isapprox(interp1(x, y, xq, 'pchip'), ...
  [17.1875 17.5 19.166666666666664 28.020833333333332], tol);
assert_isapprox(interp1(x, y, xq, 'spline'), ...
  [19.53125 16.71875 18.59375 30.15625], tol);
assert_isapprox(interp1(x, y, xq, 'linear'), [15 17.5 20 27.5], tol);
assert_isapprox(interp1(x, y, xq, 'nearest'), [20 15 25 30], tol);
%=============================================================================
