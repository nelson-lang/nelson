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
% interp3 builtin: native methods and the calls delegated to the generic path
%=============================================================================
V = reshape([1 4 2 0 3 2 -1 5 1 1 0 3 3 2 -2 4 1 0 6 2 5 -3 2 2], 2, 3, 4);
xq = [1.5 2.25 3];
yq = [1.5 1 2];
zq = [1.5 3.75 2];
%=============================================================================
asserts.isapprox(interp3(V, xq, yq, zq), [1.625 2.875 3], 1e-12);
asserts.isequal(interp3(V, xq, yq, zq, 'nearest'), [1 5 3]);
asserts.isapprox(interp3(V, xq, yq, zq, 'cubic'), [1.5625 1.734375 3], 1e-12);
asserts.isapprox(interp3(V, xq, yq, zq, 'spline'), [1.5625 1.734375 3], 1e-12);
asserts.isapprox(interp3(V, xq, yq, zq, 'makima'), [1.3577468307911338 2.1544324206067373 3], 1e-12);
asserts.isapprox(interp3(V, xq, yq, zq, "linear"), [1.625 2.875 3], 1e-12);
%=============================================================================
% outside the grid: NaN or constant value
R = interp3(V, [0.5 2.25 3], yq, zq);
asserts.istrue(isnan(R(1)));
asserts.isapprox(interp3(V, [0.5 2.25 3], yq, zq, 'linear', -7), [-7 2.875 3], 1e-12);
asserts.isapprox(interp3(V, [0.5 2.25 3], yq, zq, 'makima', 9), [9 2.1544324206067373 3], 1e-12);
%=============================================================================
% grids: decreasing, non-uniform, full grids
asserts.isapprox(interp3(3:-1:1, 1:2, 1:4, flip(V, 2), xq, yq, zq), [1.625 2.875 3], 1e-12);
asserts.isapprox(interp3([0 1 3], [0 2], [0 1 3 4], V, [0.5 2 3], [1 2 0.5], [1 2.5 4]), [1.5 2 2], 1e-12);
[X, Y, Z] = meshgrid(1:3, 1:2, 1:4);
asserts.isapprox(interp3(X, Y, Z, V, xq, yq, zq), [1.625 2.875 3], 1e-12);
%=============================================================================
% query vectors of different sizes span a grid
R = interp3(V, [1.5 2.5], [1.25; 1.75], [1 2 3]);
asserts.isequal(size(R), [2 2 3]);
[Xq, Yq, Zq] = meshgrid(xq, yq, zq);
asserts.isequal(size(interp3(V, Xq, Yq, Zq)), [3 3 3]);
asserts.isequal(size(interp3(V, Xq, Yq, Zq, 'makima')), [3 3 3]);
%=============================================================================
% pages of V
R = interp3(cat(4, V, 2 * V), xq, yq, zq);
asserts.isequal(size(R), [1 3 2]);
asserts.isapprox(R(:, :, 2), 2 * [1.625 2.875 3], 1e-12);
%=============================================================================
% classes
R = interp3(single(V), xq, yq, zq);
asserts.isequal(class(R), 'single');
R = interp3(V + 1i * V, xq, yq, zq);
asserts.isfalse(isreal(R));
asserts.isapprox(R, (1 + 1i) * [1.625 2.875 3], 1e-12);
% integer grid: generic path
R = interp3(int8(1:3), 1:2, 1:4, V, xq, yq, zq);
asserts.isequal(size(R), [1 3]);
%=============================================================================
% refinement forms
R = interp3(V);
asserts.isequal(size(R), [3 5 7]);
asserts.isequal(R(1:2:end, 1:2:end, 1:2:end), V);
%=============================================================================
% invalid calls
asserts.checkerror('interp3(V, xq, yq, zq, ''pchip'')', _('Unknown interpolation method.'), 'Nelson:interp:unknownMethod');
asserts.checkerror('interp3([1 2 2], 1:2, 1:4, V, xq, yq, zq)', _('Grid vectors must be strictly monotonic.'), 'Nelson:interpn:gridVectorsNotMonotonic');
asserts.checkerror('interp3(1:4, 1:2, 1:4, V, xq, yq, zq)', _('Grid vector length and sample value size are incompatible.'), 'Nelson:interp:gridSizeMismatch');
asserts.checkerror('interp3(V, 1, 2, 3, 4, 5)', _('Wrong number of input arguments.'), 'Nelson:wrongNumberOfInputs');
asserts.checkerror('interp3()', _('Not enough input arguments.'));
asserts.checkerror('[a, b] = interp3(V, 2, 1, 2)', _('Too many output arguments.'));
%=============================================================================
