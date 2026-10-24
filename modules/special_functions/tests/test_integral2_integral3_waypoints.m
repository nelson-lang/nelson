%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% kinks of the integrand given as waypoints: [x y] array of points
fun = @(x, y) abs(x - 0.3) .* abs(y - 0.6);
q = integral2(fun, 0, 1, 0, 1, 'Waypoints', [0.3, 0.6]);
asserts.isapprox(q, 0.29 * 0.26, 1e-10);
%=============================================================================
% same waypoints in grid vector form
q = integral2(fun, 0, 1, 0, 1, 'Waypoints', {0.3, 0.6});
asserts.isapprox(q, 0.29 * 0.26, 1e-10);
q = integral2(fun, 0, 1, 0, 1, 'Waypoints', {[0.3, 5], []});
asserts.isapprox(q, 0.29 * 0.26, 1e-6);
%=============================================================================
% discontinuous integrand over a non-rectangular region
fun = @(x, y) double(y > 0.5);
q = integral2(fun, 0, 1, 0, @(x) x, 'Waypoints', [0.5, 0.5]);
asserts.isapprox(q, 0.125, 1e-10);
%=============================================================================
q = integral2(@(x, y) x .* y, 0, 1, 0, 1, 'Waypoints', []);
asserts.isapprox(q, 0.25, 1e-10);
%=============================================================================
q = integral2(@(x, y) abs(x - 0.3) * abs(y - 0.6), 0, 1, 0, 1, 'Waypoints', [0.3, 0.6], 'Vectorized', false);
asserts.isapprox(q, 0.29 * 0.26, 1e-10);
%=============================================================================
% integral3: [x y z] array of points and grid vectors
fun = @(x, y, z) abs(x - 0.3) .* abs(y - 0.6) .* abs(z - 0.2);
q = integral3(fun, 0, 1, 0, 1, 0, 1, 'Waypoints', [0.3, 0.6, 0.2]);
asserts.isapprox(q, 0.29 * 0.26 * 0.34, 1e-9);
q = integral3(fun, 0, 1, 0, 1, 0, 1, 'Waypoints', {0.3, 0.6, 0.2}, 'Method', 'tiled');
asserts.isapprox(q, 0.29 * 0.26 * 0.34, 1e-9);
q = integral3(@(x, y, z) x + y + z, 0, 1, 0, @(x) x, 0, @(x, y) y, 'Waypoints', {0.5, 0.25, 0.1});
asserts.isapprox(q, 0.25, 1e-10);
%=============================================================================
msg = sprintf(_('''Waypoints'' value must be a %d-column array or a cell array of %d vectors with real finite values.'), 2, 2);
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''Waypoints'', [0.1, 0.2, 0.3])', msg);
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''Waypoints'', [0.1, 0.2, 0.3])', msg, 'Nelson:integral2:invalidWaypoints');
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''Waypoints'', {0.1, NaN})', msg, 'Nelson:integral2:invalidWaypoints');
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''Waypoints'', [1i, 0])', msg, 'Nelson:integral2:invalidWaypoints');
msg = sprintf(_('''Waypoints'' value must be a %d-column array or a cell array of %d vectors with real finite values.'), 3, 3);
asserts.checkerror('integral3(@(x, y, z) x, 0, 1, 0, 1, 0, 1, ''Waypoints'', {0.1, 0.2})', msg, 'Nelson:integral3:invalidWaypoints');
%=============================================================================
