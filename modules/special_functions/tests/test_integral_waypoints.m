%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documented example: complex contour integration using waypoints
fun = @(z) 1 ./ (2 * z - 1);
q = integral(fun, 0, 0, 'Waypoints', [1 + 1i, 1 - 1i]);
asserts.isapprox(q, -pi * 1i, 1e-10, 1e-10);
%=============================================================================
% same contour with an array-valued integrand
q = integral(@(z) [1, 2] ./ (2 * z - 1), 0, 0, 'Waypoints', [1 + 1i, 1 - 1i], 'ArrayValued', true);
asserts.isapprox(q, [-pi * 1i, -2 * pi * 1i], 1e-10, 1e-10);
%=============================================================================
% kink of the integrand given as a waypoint
q = integral(@(x) abs(x - 0.3), 0, 1, 'Waypoints', 0.3);
asserts.isapprox(q, 0.29, 1e-12);
%=============================================================================
% discontinuity given as a waypoint
q = integral(@(x) double(x > 1 / 3), 0, 1, 'Waypoints', 1 / 3);
asserts.isapprox(q, 2 / 3, 1e-12);
%=============================================================================
% order, duplicates, orientation and out-of-range waypoints do not matter
q = integral(@(x) x, 0, 1, 'Waypoints', [0.7, 0.3, 0.3]);
asserts.isapprox(q, 0.5, 1e-12);
q = integral(@(x) x, 0, 1, 'Waypoints', [0.5; 0.25]);
asserts.isapprox(q, 0.5, 1e-12);
q = integral(@(x) x, 1, 0, 'Waypoints', [0.3, 0.7]);
asserts.isapprox(q, -0.5, 1e-12);
q = integral(@(x) x, 0, 1, 'Waypoints', [-2, 2]);
asserts.isapprox(q, 0.5, 1e-12);
q = integral(@(x) x, 0, 1, 'Waypoints', []);
asserts.isapprox(q, 0.5, 1e-12);
%=============================================================================
% infinite limits with waypoints
q = integral(@(x) exp(-x), 0, Inf, 'Waypoints', [1, 2]);
asserts.isapprox(q, 1, 1e-9);
q = integral(@(x) exp(-x .^ 2), -Inf, Inf, 'Waypoints', 0);
asserts.isapprox(q, sqrt(pi), 1e-9);
%=============================================================================
% complex limits: straight line path
q = integral(@(z) z, 0, 1 + 1i);
asserts.isapprox(q, (1 + 1i) ^ 2 / 2, 1e-10, 1e-10);
q = integral(@(z) exp(z), 1i, 2);
asserts.isapprox(q, exp(2) - exp(1i), 1e-10, 1e-10);
%=============================================================================
asserts.checkerror('integral(@(x) x, 0, 1, ''Waypoints'', NaN)', _('''Waypoints'' value must be a vector with all finite values.'));
asserts.checkerror('integral(@(x) x, 0, 1, ''Waypoints'', NaN)', _('''Waypoints'' value must be a vector with all finite values.'), 'Nelson:integral:invalidWaypoints');
asserts.checkerror('integral(@(x) x, 0, 1, ''Waypoints'', [0.2 0.4; 0.6 0.8])', _('''Waypoints'' value must be a vector with all finite values.'), 'Nelson:integral:invalidWaypoints');
asserts.checkerror('integral(@(x) x, 0, 1, ''Waypoints'', {0.5})', _('''Waypoints'' value must be a vector with all finite values.'), 'Nelson:integral:invalidWaypoints');
asserts.checkerror('integral(@(x) x, 0, Inf, ''Waypoints'', 1i)', _('Contour endpoints and waypoints must be finite.'));
asserts.checkerror('integral(@(x) x, 0, Inf, ''Waypoints'', 1i)', _('Contour endpoints and waypoints must be finite.'), 'Nelson:integral:nonFiniteContourError');
%=============================================================================
