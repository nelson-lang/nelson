%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% default: 100 points, closed boundary
[lat, lon] = ellipse1(0, 0, [5 0.8]);
assert_isequal(size(lat), [100 1]);
assert_isequal(size(lon), [100 1]);
assert_istrue(abs(lat(1) - lat(end)) < 1e-9);
assert_istrue(abs(lon(1) - lon(end)) < 1e-9);
%=============================================================================
% zero eccentricity reduces to a small circle: every point is at spherical
% range equal to the semimajor axis from the center
[lat, lon] = ellipse1(0, 0, [5 0], 0, 37);
assert_isequal(size(lat), [37 1]);
% great-circle distance from the center (0,0) back to each boundary point
d = rad2deg(acos(min(1, cosd(lat) .* cosd(lon))));
assert_isapprox(d, 5 * ones(37, 1), 1e-6);
%=============================================================================
% custom number of points
[lat, lon] = ellipse1(10, 20, [3 0.5], 0, 50);
assert_isequal(size(lat), [50 1]);
assert_isequal(size(lon), [50 1]);
%=============================================================================
% ellipse must be [semimajor eccentricity]
assert_checkerror('ellipse1(0, 0, [1 2 3])', _('ELLIPSE must be [semimajor eccentricity].'));
%=============================================================================
% narginchk: at least three arguments
assert_checkerror('ellipse1(0, 0)', _('Not enough input arguments.'));
%=============================================================================
