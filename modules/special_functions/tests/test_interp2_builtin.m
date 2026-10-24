%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
% interp2 builtin: native methods and the calls delegated to the generic path
%=============================================================================
V = [1 4 2 0 3; 2 -1 5 1 1; 0 3 3 2 -2; 4 1 0 6 2];
xq = [1.5 2.25 4.5];
yq = [1.5 3.75 2];
%=============================================================================
asserts.isapprox(interp2(V, xq, yq), [1.5 1.3125 1], 1e-12);
asserts.isequal(interp2(V, xq, yq, 'nearest'), [1 1 1]);
asserts.isapprox(interp2(V, xq, yq, 'cubic'), [-1.08984375 2.25140380859375 -0.375], 1e-12);
asserts.isapprox(interp2(V, xq, yq, 'spline'), [-1.08984375 2.25140380859375 -0.375], 1e-12);
asserts.isapprox(interp2(V, xq, yq, 'makima'), [0.53865971549854852 1.4923922613236593 0.63602941176470584], 1e-12);
asserts.isapprox(interp2(V, xq, yq, "linear"), [1.5 1.3125 1], 1e-12);
%=============================================================================
% outside the grid: NaN, constant value, spline extrapolates
R = interp2(V, [0.5 2.25 6], yq);
asserts.istrue(isnan(R(1)) && isnan(R(3)));
asserts.isapprox(interp2(V, [0.5 2.25 6], yq, 'linear', -7), [-7 1.3125 -7], 1e-12);
asserts.isapprox(interp2(V, [0.5 2.25 6], yq, 'makima', 9), [9 1.4923922613236593 9], 1e-12);
asserts.isapprox(interp2(V, [0.5 2.25 6], yq, 'spline'), [12.08984375 2.25140380859375 19], 1e-12);
%=============================================================================
% grids: decreasing, non-uniform, full grids
asserts.isapprox(interp2(5:-1:1, 1:4, fliplr(V), xq, yq), [1.5 1.3125 1], 1e-12);
asserts.isapprox(interp2([0 1 3 4 7], [0 2 3 6], V, [0.5 2 5], [1 2.5 4]), [1.5 2.5 2], 1e-12);
[X, Y] = meshgrid(1:5, 1:4);
asserts.isapprox(interp2(X, Y, V, xq, yq), [1.5 1.3125 1], 1e-12);
%=============================================================================
% query vectors of different sizes span a grid
R = interp2(V, [1.5 3.5], [1.25; 2.5; 3.75]);
asserts.isequal(size(R), [3 2]);
asserts.isapprox(R, [2 1.5; 1 2.75; 2.25 2.875], 1e-12);
[Xq, Yq] = meshgrid(xq, yq);
asserts.isequal(size(interp2(V, Xq, Yq)), [3 3]);
asserts.isequal(size(interp2(V, Xq, Yq, 'makima')), [3 3]);
%=============================================================================
% pages of V
R = interp2(cat(3, V, 2 * V), xq, yq);
asserts.isequal(size(R), [1 3 2]);
asserts.isapprox(R(:, :, 2), 2 * [1.5 1.3125 1], 1e-12);
%=============================================================================
% classes
R = interp2(single(V), xq, yq);
asserts.isequal(class(R), 'single');
R = interp2(V + 1i * V, xq, yq);
asserts.isfalse(isreal(R));
asserts.isapprox(R, (1 + 1i) * [1.5 1.3125 1], 1e-12);
% integer grid: generic path
R = interp2(int8(1:5), 1:4, V, xq, yq);
asserts.isequal(size(R), [1 3]);
%=============================================================================
% refinement forms
R = interp2(V);
asserts.isequal(size(R), [7 9]);
asserts.isequal(R(1:2:end, 1:2:end), V);
asserts.isequal(size(interp2(V, 2)), [13 17]);
%=============================================================================
% invalid calls
asserts.checkerror('interp2(V, xq, yq, ''pchip'')', _('Unknown interpolation method.'), 'Nelson:interp:unknownMethod');
asserts.checkerror('interp2([1 2 2 3 4], 1:4, V, xq, yq)', _('Grid vectors must be strictly monotonic.'), 'Nelson:interpn:gridVectorsNotMonotonic');
asserts.checkerror('interp2(1:6, 1:4, V, xq, yq)', _('Grid vector length and sample value size are incompatible.'), 'Nelson:interp:gridSizeMismatch');
asserts.checkerror('interp2(V, 1, 2, 3)', _('Wrong number of input arguments.'), 'Nelson:wrongNumberOfInputs');
asserts.checkerror('interp2()', _('Not enough input arguments.'));
asserts.checkerror('[a, b] = interp2(V, 2, 2)', _('Too many output arguments.'));
%=============================================================================
