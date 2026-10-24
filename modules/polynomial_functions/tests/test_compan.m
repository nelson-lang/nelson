%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
assert_isequal(compan([1 -6 11 -6]), [6 -11 6; 1 0 0; 0 1 0]);
%=============================================================================
% the leading coefficient scales the first row
assert_isequal(compan([2 -4 2]), [2 -1; 1 0]);
%=============================================================================
assert_isequal(compan([1 0 -1]), [0 1; 1 0]);
%=============================================================================
% two coefficients give a 1x1 matrix
assert_isequal(compan([1 5]), -5);
%=============================================================================
% a single coefficient gives an empty matrix
assert_isequal(compan(3), []);
%=============================================================================
% a column vector is accepted like a row vector
assert_isequal(compan([1; -6; 11; -6]), [6 -11 6; 1 0 0; 0 1 0]);
%=============================================================================
% the eigenvalues of the companion matrix are the polynomial roots
assert_isapprox(sort(eig(compan([1 -6 11 -6]))), [1; 2; 3], 1e-10);
%=============================================================================
assert_checkerror('compan([])', _('Input argument must be a vector.'));
%=============================================================================
