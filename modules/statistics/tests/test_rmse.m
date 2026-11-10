%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isapprox(rmse([1 2 3], [1 2 4]), 0.577350269189626, 1e-12);
assert_isapprox(rmse([1 2; 3 4], [2 2; 3 6]), [0.707106781186548 1.414213562373095], 1e-12);
assert_isapprox(rmse([1 2; 3 4], [2 2; 3 6], 2), [0.707106781186548; 1.414213562373095], 1e-12);
assert_isapprox(rmse([1 2; 3 4], [2 2; 3 6], 'all'), 1.118033988749895, 1e-12);
assert_istrue(isnan(rmse([1 NaN 3], [1 2 4])));
assert_isapprox(rmse([1 NaN 3], [1 2 4], 'omitnan'), 0.707106781186548, 1e-12);
assert_istrue(isnan(rmse([1 NaN], [1 2], 'includenan')));
assert_isequal(class(rmse(single([1 2 3]), [1 2 4])), 'single');
assert_isapprox(rmse([1 2 3], [1 2 4], 'Weights', [1 1 2]), 0.707106781186548, 1e-12);
assert_istrue(isnan(rmse([], [])));
assert_isapprox(rmse([1 2; 3 4], [1 2]), [1.414213562373095 1.414213562373095], 1e-12);
assert_checkerror('rmse(int8([1 2]), int8([1 3]))', _('Inputs must be single or double arrays.'));
%=============================================================================
