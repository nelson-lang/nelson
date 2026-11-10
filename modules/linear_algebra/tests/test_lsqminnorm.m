%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isapprox(lsqminnorm([4 1; 2 3], [1; 2]), [0.1; 0.6], 1e-12);
assert_isapprox(lsqminnorm([1 2; 2 4], [1; 2]), [0.2; 0.4], 1e-12);
assert_isapprox(lsqminnorm([1 2 3; 4 5 6], [1; 2]), [-1/18; 1/9; 5/18], 1e-12);
assert_isapprox(lsqminnorm([1 2; 3 4; 5 6], [1; 2; 4]), [2/3; 1/12], 1e-12);
assert_isapprox(lsqminnorm([1 2; 2 4.0001], [1; 2], 1e-3), [0.199994; 0.399995], 1e-5);
assert_isapprox(lsqminnorm([1 2; 3 4; 5 6], [1 2; 2 3; 4 5]), [2/3 -1/3; 1/12 13/12], 1e-12);
assert_isequal(lsqminnorm([1 0; 0 3e-16], [1; 1]), [1; 0]);
assert_isequal(class(lsqminnorm(single([1 2; 3 4]), [1; 2])), 'single');
x = lsqminnorm(sparse([1 2; 3 4]), [1; 2]);
assert_isfalse(issparse(x));
assert_isequal(lsqminnorm(zeros(2, 3), [1; 2]), zeros(3, 1));
assert_checkerror('lsqminnorm(int8([1 2; 3 4]), [1; 2])', _('Inputs must be single or double arrays.'));
assert_checkerror('lsqminnorm([1 2; 3 4], [1; 2; 3])', _('The two inputs must have the same number of rows.'));
%=============================================================================
