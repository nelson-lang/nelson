%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A solid-rotation field has an exact, analytically known curl, independent of
% any implementation detail: U = -Y, V = X gives curl_z = 2 and angular
% velocity 1 everywhere.
%=============================================================================
[x, y] = meshgrid(-3:3, -3:3);
U = -y;
V = x;
[cz, cav] = curl(x, y, U, V);
assert_isapprox(cz, 2 * ones(size(cz)), 1e-10);
assert_isapprox(cav, ones(size(cav)), 1e-10);
%=============================================================================
% single output returns curl_z (2-D)
czOnly = curl(x, y, U, V);
assert_isapprox(czOnly, 2 * ones(size(czOnly)), 1e-10);
%=============================================================================
% default grid (no X, Y)
[cz2, ~] = curl(U, V);
assert_isapprox(cz2, 2 * ones(size(cz2)), 1e-10);
%=============================================================================
% 3-D rotation about z: curl = (0, 0, 2), angular velocity 1
[X, Y, Z] = meshgrid(-3:3, -3:3, -3:3);
U3 = -Y;
V3 = X;
W3 = zeros(size(X));
[cx, cy, cz3, cav3] = curl(X, Y, Z, U3, V3, W3);
assert_isapprox(cx, zeros(size(cx)), 1e-10);
assert_isapprox(cy, zeros(size(cy)), 1e-10);
assert_isapprox(cz3, 2 * ones(size(cz3)), 1e-10);
assert_isapprox(cav3, ones(size(cav3)), 1e-10);
%=============================================================================
% 3-D rotation about x: U=0, V=-Z, W=Y gives curl = (2, 0, 0)
U4 = zeros(size(X));
V4 = -Z;
W4 = Y;
[cx4, cy4, cz4] = curl(X, Y, Z, U4, V4, W4);
assert_isapprox(cx4, 2 * ones(size(cx4)), 1e-10);
assert_isapprox(cy4, zeros(size(cy4)), 1e-10);
assert_isapprox(cz4, zeros(size(cz4)), 1e-10);
%=============================================================================
% mismatched sizes error
assert_checkerror('curl(ones(3), ones(4))', 'X, Y, U and V must have the same size.');
%=============================================================================
