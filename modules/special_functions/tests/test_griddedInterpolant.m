%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 1-D default (linear) interpolation.
F = griddedInterpolant([1 2 3], [10 20 30]);
assert_isequal(F(2.5), 25);
assert_isequal(evaluate(F, 2.5), 25);
assert_isequal(F.Method, 'linear');
assert_isequal(F.ExtrapolationMethod, 'linear');
assert_isequal(F.GridVectors, {[1 2 3]});
assert_isequal(F.Values, [10 20 30]);
%=============================================================================
% 1-D spline.
F = griddedInterpolant([1 2 3], [10 20 30], 'spline');
assert_isequal(F(2.5), 25);
assert_isequal(F.ExtrapolationMethod, 'spline');
%=============================================================================
% N-D grid given as a cell of grid vectors.
F = griddedInterpolant({1:3, 1:3}, magic(3));
assert_isequal(F(2, 2), 5);
assert_isequal(F.Method, 'linear');
assert_isequal(F.ExtrapolationMethod, 'linear');
%=============================================================================
% Default ExtrapolationMethod matches Method.
methodList = {'linear', 'nearest', 'next', 'previous', 'pchip', 'cubic', 'spline', 'makima'};
for k = 1:numel(methodList)
  F = griddedInterpolant([1 2 3], [10 20 30], methodList{k});
  assert_isequal(F.Method, methodList{k});
  assert_isequal(F.ExtrapolationMethod, methodList{k});
end
%=============================================================================
% Extrapolation modes in 1-D.
F = griddedInterpolant([1 2 3], [10 20 30], 'linear', 'none');
assert_isequal(F(5), NaN);
F = griddedInterpolant([1 2 3], [10 20 30], 'linear', 'linear');
assert_isequal(F(5), 50);
F = griddedInterpolant([1 2 3], [10 20 30], 'linear', 'nearest');
assert_isequal(F(5), 30);
%=============================================================================
% previous / next extrapolation returns NaN in the unavailable direction.
F = griddedInterpolant([1 2 3 4 5], [3 -1 4 1 5], 'next');
assert_isequal(F([-1 0 6 7]), [3 3 NaN NaN]);
F = griddedInterpolant([1 2 3 4 5], [3 -1 4 1 5], 'previous');
assert_isequal(F([-1 0 6 7]), [NaN NaN 5 5]);
%=============================================================================
% 1-D 'cubic' extrapolates via cubic convolution (default ExtrapolationMethod
% is 'cubic'); interior queries stay exact.
F = griddedInterpolant([1 2 3 4 5], [3 -1 4 1 5], 'cubic');
assert_isequal(F.ExtrapolationMethod, 'cubic');
assert_isapprox(F([-1 0 6 7]), [38 16 16 34], 1e-11);
assert_isapprox(F([1.5 2.5 3.5]), [-0.125 1.4375 2.5625], 1e-11);
assert_isapprox(F([-1 1.5 6 3.5 7]), [38 -0.125 16 2.5625 34], 1e-11);
%=============================================================================
% Value-only constructor uses default unit-spaced grids.
F = griddedInterpolant([10 20 30 40]);
assert_isequal(F(2.5), 25);
assert_isequal(F.GridVectors, {[1 2 3 4]});
%=============================================================================
% Column inputs preserve orientation.
F = griddedInterpolant([1; 2; 3], [10; 20; 30]);
assert_isequal(size(F.GridVectors{1}), [3 1]);
assert_isequal(size(F.Values), [3 1]);
assert_isequal(F(2.5), 25);
%=============================================================================
% Query shape is preserved.
F = griddedInterpolant([1 2 3], [10 20 30]);
assert_isequal(size(F([1.5 2.5])), [1 2]);
assert_isequal(size(F([1.5; 2.5])), [2 1]);
%=============================================================================
% 2-D interpolation, elementwise and grid-vector queries.
F = griddedInterpolant({1:3, 1:3}, magic(3));
assert_isequal(F([1 2; 2 3], [1 1; 2 2]), [8 3; 5 9]);
R = F({[1 2], [1 2 3]});
assert_isequal(size(R), [2 3]);
assert_isequal(R, [8 1 6; 3 5 7]);
%=============================================================================
% 2-D nearest tie-breaking rounds toward the larger sample point.
[X, Y] = ndgrid(1:4, 1:3);
V = X .^ 2 + 2 * Y;
F = griddedInterpolant({1:4, 1:3}, V, 'nearest');
assert_isequal(F([1.5 2.5 3.9], [1.2 2.2 2.9]), [6 13 22]);
%=============================================================================
% 2-D linear extrapolation.
F = griddedInterpolant({1:3, 1:3}, magic(3), 'linear', 'linear');
assert_isequal(F(5, 5), -58);
F = griddedInterpolant({1:3, 1:3}, magic(3), 'linear', 'none');
assert_isequal(F(5, 5), NaN);
assert_isequal(F(2, 2), 5);
%=============================================================================
% 3-D interpolation.
[A, B, C] = ndgrid(1:3, 1:3, 1:3);
V3 = A + 10 * B + 100 * C;
F = griddedInterpolant({1:3, 1:3, 1:3}, V3, 'linear');
assert_isequal(F(1.5, 2.5, 2.5), 276.5);
%=============================================================================
% ndgrid full-array constructor form.
[X, Y] = ndgrid(1:3, 1:3);
F = griddedInterpolant(X, Y, magic(3));
assert_isequal(F.GridVectors, {[1 2 3], [1 2 3]});
assert_isequal(F(2, 2), 5);
%=============================================================================
% Properties are settable and re-validated.
F = griddedInterpolant([1 2 3], [10 20 30]);
F.Method = 'nearest';
assert_isequal(F(2.4), 20);
assert_isequal(F(2.6), 30);
F.Values = [0 5 0];
assert_isequal(F(2), 5);
F.GridVectors = {[0 1 2]};
assert_isequal(F(1), 5);
%=============================================================================
% Invalid method and extrapolation method are rejected.
msg = _('The interpolation method must be ''linear'', ''nearest'', ''next'', ''previous'', ''pchip'', ''cubic'', ''spline'', or ''makima''.');
assert_checkerror('griddedInterpolant([1 2 3], [10 20 30], ''bogus'')', msg);
F = griddedInterpolant([1 2 3], [10 20 30]);
assert_checkerror('F.Method = ''bogus'';', msg);
msg = _('The extrapolation method must be ''linear'', ''nearest'', ''next'', ''previous'', ''pchip'', ''cubic'', ''spline'', ''makima'', or ''none''.');
assert_checkerror('griddedInterpolant([1 2 3], [10 20 30], ''linear'', ''bogus'')', msg);
%=============================================================================
% Non-monotonic grid is rejected.
assert_checkerror('griddedInterpolant([3 1 2], [10 20 30])', _('Sample points must be sorted in ascending order.'));
%=============================================================================
% Grid length must match the sample values.
assert_checkerror('griddedInterpolant([1 2 3], [10 20 30 40])', _('Sample points vector corresponding to grid dimension 1 must contain 4 elements.'));
%=============================================================================
% N-D 'makima' delegates to the tensor-product modified-Akima evaluator and
% matches expected behavior for both interior and extrapolated queries.
[X, Y] = ndgrid(1:4, 1:3);
V = X .^ 2 + 2 * Y;
F = griddedInterpolant({1:4, 1:3}, V, 'makima');
Ri = F([1.5 2.5 3.9], [1.2 2.2 2.9]);
assert_isapprox(Ri(:).', [4.629166666666667, 10.639583333333334, 21.02395], 1e-11);
Rx = F([0.5 4.5], [0.5 3.5]);
assert_isapprox(Rx(:).', [1.9375, 26.93125], 1e-11);
%=============================================================================
% N-D 'cubic' extrapolates via tensor-product cubic convolution (clamped end
% interval plus extended endpoint values); interior queries stay exact and the
% out-of-grid queries return finite values instead of NaN.
[X, Y] = ndgrid(1:4, 1:3);
V = X .^ 2 + 2 * Y;
F = griddedInterpolant({1:4, 1:3}, V, 'cubic');
assert_isequal(F.ExtrapolationMethod, 'cubic');
Ri = F([1.5 2.5 3.9], [1.2 2.2 2.9]);
assert_isapprox(Ri(:).', [4.65, 10.65, 21.01], 1e-11);
Rx = F([0.5 4.5], [0.5 3.5]);
assert_isapprox(Rx(:).', [1.25, 27.25], 1e-11);
%=============================================================================
% 3-D 'cubic' extrapolation via the tensor-product scheme.
[A, B, C] = ndgrid(1:4, 1:3, 1:3);
V3 = A .^ 2 + 2 * B + 3 * C;
F3 = griddedInterpolant({1:4, 1:3, 1:3}, V3, 'cubic');
R3 = F3([0.5 4.5], [0.5 3.5], [0.5 3.5]);
assert_isapprox(R3(:).', [2.75, 37.75], 1e-11);
%=============================================================================
% Non-uniform grid falls back to spline for the 'cubic' extrapolation, matching
% the interior fallback (a quadratic is reproduced exactly by both).
xg = [1 2 4 7];
[Xn, Yn] = ndgrid(xg, 1:3);
Vn = Xn .^ 2 + 2 * Yn;
Fn = griddedInterpolant({xg, 1:3}, Vn, 'cubic');
assert_isapprox(Fn(0.5, 0.5), 1.25, 1e-10);
%=============================================================================
