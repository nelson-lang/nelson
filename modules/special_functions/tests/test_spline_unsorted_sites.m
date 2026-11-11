%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sample points in any order are sorted with their values
assert_isapprox(spline([3 1 2], [9 1 4], 2.5), 6.25, 1e-12);
assert_isapprox(spline([3 2 1 0], [9 4 1 0], [0.5 2.5]), [0.25 6.25], 1e-12);
assert_isapprox(spline([2 1], [4 1], 1.5), 2.5, 1e-12);
assert_isapprox(spline([3 1 2 4], [9 1 4 16], [1.5 3.5]), [2.25 12.25], 1e-12);
pp = spline([3 1 2 4], [9 1 4 16]);
assert_isequal(pp.breaks, [1 2 3 4]);
%=============================================================================
% NaN points are dropped with a warning
lastwarn('');
assert_isapprox(spline([1 NaN 2 3], [1 5 4 9], 2.5), 6.25, 1e-12);
[msg, id] = lastwarn();
assert_isequal(id, 'Nelson:spline:nanIgnored');
assert_isequal(msg, _('NaN sample points and their values were ignored.'));
assert_isapprox(spline([1 2 3 4], [1 NaN 9 16], 2.5), 6.25, 1e-12);
%=============================================================================
assert_checkerror('spline([1 1 2], [1 2 3], 1.5)', _('Sample points must be distinct.'), 'Nelson:spline:repeatedSites');
%=============================================================================
