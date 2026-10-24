%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
[x, y, z] = ellipsoid(0, 0, 0, 1, 1, 1, 20);
assert_isequal(size(x), [21 21]);
assert_isequal(size(y), [21 21]);
assert_isequal(size(z), [21 21]);
%=============================================================================
% equal radii: every vertex is on the unit sphere
r = sqrt(x .^ 2 + y .^ 2 + z .^ 2);
assert_istrue(max(abs(r(:) - 1)) < 1e-12);
%=============================================================================
% radii and center: vertices satisfy the ellipsoid equation
[x2, y2, z2] = ellipsoid(10, 20, 30, 2, 3, 4, 12);
eqn = ((x2(:) - 10) / 2) .^ 2 + ((y2(:) - 20) / 3) .^ 2 + ((z2(:) - 30) / 4) .^ 2;
assert_istrue(max(abs(eqn - 1)) < 1e-12);
%=============================================================================
% default N is 20
[x3, y3, z3] = ellipsoid(0, 0, 0, 1, 2, 3);
assert_isequal(size(x3), [21 21]);
%=============================================================================
% no output argument plots a surface
f = figure();
ellipsoid(0, 0, 0, 1, 2, 3, 15);
assert_isequal(numel(findobj(f, 'Type', 'surface')), 1);
close(f);
%=============================================================================
