%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% heading north from the equator advances latitude
[lat, lon] = reckon(0, 0, 10, 0);
assert_isapprox(lat, 10, 1e-9);
assert_istrue(abs(lon) < 1e-9);
%=============================================================================
% heading east from the equator advances longitude
[lat, lon] = reckon(0, 0, 10, 90);
assert_istrue(abs(lat) < 1e-9);
assert_isapprox(lon, 10, 1e-9);
%=============================================================================
% heading south from the equator decreases latitude
[lat, lon] = reckon(0, 0, 10, 180);
assert_isapprox(lat, -10, 1e-9);
assert_istrue(abs(lon) < 1e-9);
%=============================================================================
% vectorized range/azimuth broadcast against scalar start point
[lat, lon] = reckon(0, 0, [10; 10; 10; 10], [0; 90; 180; 270]);
assert_isequal(size(lat), [4 1]);
assert_istrue(max(abs(lat - [10; 0; -10; 0])) < 1e-9);
assert_istrue(max(abs(lon - [0; 10; 0; -10])) < 1e-9);
%=============================================================================
% narginchk: all four arguments are required
assert_checkerror('reckon(0, 0, 10)', _('Not enough input arguments.'));
%=============================================================================
