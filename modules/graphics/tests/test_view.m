%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
plot(sin(0:0.1:2*pi))
ax = gca();
assert_isequal(ax.View, [0 90]);
assert_isapprox(ax.CameraTarget, [32.0000   -0.0002   -0.0500], 1e-4)
assert_isapprox(ax.CameraPosition, [ 32.0000   -0.0002    1.9000], 1e-4);
assert_isequal(ax.CameraUpVector, [0 1 0]);
%=============================================================================
f = figure();
surf(peaks);
ax = gca();
assert_isapprox(ax.View, [ -37.5000   30.0000], 1e-4);
v = view();
assert_isapprox(v, [ -37.5000   30.0000], 1e-4);
[az, el] = view();
assert_isapprox(az, -37.5000, 1e-4);
assert_isapprox(el, 30.0000, 1e-4);
assert_isapprox(ax.CameraTarget, [25.0000   25.0000    0.7643], 1e-4)
assert_isapprox(ax.CameraPosition, [6.6957    1.1454   18.1241], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
f = figure();
f.ToolBar = 'none';
[X, Y, Z] = peaks();
surf(X, Y, Z);
ax = gca();
% To change
assert_isapprox(ax.View, [ -37.5000   30.0000], 1e-4);
assert_isapprox(ax.CameraTarget, [ 0         0    0.7643], 1e-4)
assert_isapprox(ax.CameraPosition, [ -4.4563   -5.8076    4.9907], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
view(90,0)
assert_isapprox(ax.View, [ 90   0], 1e-4);
assert_isapprox(ax.CameraTarget, [ 0         0    0.7643], 1e-4)
assert_isapprox(ax.CameraPosition, [8.4528   -0.0000    0.7643], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
view(2)
assert_isapprox(ax.View, [0   90], 1e-4);
assert_isapprox(ax.CameraTarget, [ 0         0    0.7643], 1e-4)
assert_isapprox(ax.CameraPosition, [ 0         0    9.0752], 1e-4);
assert_isequal(ax.CameraUpVector, [0 1 0]);
%=============================================================================
view([30, 50, 70])
assert_isapprox(ax.View, [149.0362   50.2059], 1e-4);
assert_isapprox(ax.CameraTarget, [ 0         0    0.7643], 1e-4)
assert_isapprox(ax.CameraPosition, [   2.7834    4.6391    7.2590], 1e-4);
assert_isequal(ax.CameraUpVector, [0 0 1]);
%=============================================================================
 