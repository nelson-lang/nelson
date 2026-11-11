%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unordered sample points are sorted with their values
assert_isapprox(interp1([3 1 2], [9 1 4], 2.5), 6.5, 1e-12);
assert_isapprox(interp1([3 1 2], [9 1 4], 2.5, 'spline'), 6.25, 1e-12);
assert_isapprox(interp1([3 1 2], [9 1 4], 2.5, 'pchip'), 6.21875, 1e-12);
assert_isapprox(pchip([3 1 2], [9 1 4], 2.5), 6.21875, 1e-12);
assert_isapprox(makima([3 1 2 4], [9 1 4 16], 2.5), 6.2395833, 1e-7);
assert_isapprox(interp1([3 1 2], [9 1; 1 2; 4 3], 2.5), [6.5 2], 1e-12);
assert_isapprox(interp1([3 2 1], [9 4 1], [1.5 2.5]), [2.5 6.5], 1e-12);
%=============================================================================
msg = _('Sample points must be distinct.');
assert_checkerror('interp1([1 1 2], [1 2 3], 1.5)', msg, 'Nelson:interp1:repeatedSamplePoints');
%=============================================================================
