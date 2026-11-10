%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a logical 'native' mean is the saturated logical sum over the count: any(X) / n
L = logical([1 0; 0 0; 1 1]);
%=============================================================================
R = mean(true(2, 3), 'native');
assert_isequal(class(R), 'double');
assert_isequal(R, [0.5 0.5 0.5]);
assert_isequal(mean(true(2, 3), 1, 'native'), [0.5 0.5 0.5]);
assert_isequal(mean(true(2, 3), 2, 'native'), [1/3; 1/3]);
assert_isequal(mean(true(2, 3), 'all', 'native'), 1/6);
assert_isequal(mean(true(2, 3, 4), [1 3], 'native'), [0.125 0.125 0.125]);
assert_isequal(mean(true(2, 3), [2 1], 'native'), 1/6);
assert_isequal(mean(logical([1 0 1 1]), 'native'), 0.25);
assert_isequal(mean(logical([0 0 0]), 'native'), 0);
%=============================================================================
assert_isequal(mean(L, 'native'), [1/3 1/3]);
assert_isequal(mean(L, 2, 'native'), [0.5; 0; 0.5]);
assert_isequal(mean(L, [1 2], 'native'), 1/6);
assert_isequal(mean(L, 'native', 'omitnan'), [1/3 1/3]);
assert_isequal(mean(L, 'omitnan', 'native'), [1/3 1/3]);
assert_isequal(mean(L, 'all', 'native', 'omitnan'), 1/6);
assert_isequal(mean(L, [1 2], 'omitnan', 'native'), 1/6);
assert_isequal(mean(logical([1 0 1]), 'native', 'includenan'), 1/3);
%=============================================================================
assert_isequal(mean(true, 'native'), 1);
assert_isequal(mean(true(2, 3), 3, 'native'), ones(2, 3));
assert_isequal(mean(true(2, 3), 5, 'native'), ones(2, 3));
%=============================================================================
assert_isequal(mean(logical([]), 'native'), NaN);
assert_isequal(mean(false(0, 3), 'native'), NaN(1, 3));
assert_isequal(mean(false(3, 0), 'native'), zeros(1, 0));
assert_isequal(mean(false(0, 3), 2, 'native'), zeros(0, 1));
assert_isequal(mean(false(0, 3), 'all', 'native'), NaN);
%=============================================================================
S = sparse(logical([1 0; 1 1]));
R = mean(S, 'native');
assert_istrue(issparse(R));
assert_isequal(class(R), 'double');
assert_isequal(full(R), [0.5 0.5]);
assert_isequal(full(mean(S, 2, 'native')), [0.5; 0.5]);
R = mean(S, 'all', 'native');
assert_istrue(issparse(R));
assert_isequal(full(R), 0.25);
assert_isequal(full(mean(S, [1 2], 'native')), 0.25);
assert_isequal(full(mean(S)), [1 0.5]);
R = mean(sparse(false(0, 3)), 'native');
assert_istrue(issparse(R));
assert_isequal(full(R), NaN(1, 3));
%=============================================================================
% 'double' and 'default' keep the true mean
assert_isapprox(mean(logical([1 0 1]), 'double'), 2/3, 1e-15);
assert_isapprox(mean(logical([1 0 1]), 'default'), 2/3, 1e-15);
assert_isequal(mean(logical([1 0 1 1])), 0.75);
%=============================================================================
