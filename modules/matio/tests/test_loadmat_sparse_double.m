%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
clear testsparse
M = [     1     2     3     4     5;
2     0     0     0     0;
3     0     0     0     0];
testsparse_ref = sparse(M);
loadmat([mat_dir, 'test_sparse_4.2c_SOL2.mat']);
assert_isequal(testsparse, testsparse_ref);
%=============================================================================
clear testsparse
loadmat([mat_dir, 'test_sparse_6.1_SOL2.mat']);
assert_isequal(testsparse, testsparse_ref);
%=============================================================================
clear testsparse
loadmat([mat_dir, 'test_sparse_6.5.1_GLNX86.mat']);
assert_isequal(testsparse, testsparse_ref);
%=============================================================================
clear testsparse
loadmat([mat_dir, 'test_sparse_7.1_GLNX86.mat']);
assert_isequal(testsparse, testsparse_ref);
%=============================================================================
clear testsparse
loadmat([mat_dir, 'test_sparse_7.4_GLNX86.mat']);
assert_isequal(testsparse, testsparse_ref);
%=============================================================================
clear testsparse
M = [1.0000 + 1.0000i   2.0000 + 0.0000i   3.0000 + 0.0000i   4.0000 + 0.0000i   5.0000 + 0.0000i;
2.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i;
3.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i   0.0000 + 0.0000i];
testsparsecomplex_ref = sparse(M);
clear testsparsecomplex
loadmat([mat_dir, 'test_sparsecomplex_4.2c_SOL2.mat']);
assert_isequal(testsparsecomplex, testsparsecomplex_ref);
%=============================================================================
clear testsparsecomplex
loadmat([mat_dir, 'test_sparsecomplex_6.1_SOL2.mat']);
assert_isequal(testsparsecomplex, testsparsecomplex_ref);
%=============================================================================
clear testsparsecomplex
loadmat([mat_dir, 'test_sparsecomplex_6.5.1_GLNX86.mat']);
assert_isequal(testsparsecomplex, testsparsecomplex_ref);
%=============================================================================
clear testsparsecomplex
loadmat([mat_dir, 'test_sparsecomplex_7.1_GLNX86.mat']);
assert_isequal(testsparsecomplex, testsparsecomplex_ref);
%=============================================================================
clear testsparsecomplex
loadmat([mat_dir, 'test_sparsecomplex_7.4_GLNX86.mat']);
assert_isequal(testsparsecomplex, testsparsecomplex_ref);
%=============================================================================
M = [1.0000         0    2.0000         0   -3.5000         0];
testsparsefloat_ref = sparse(M);
loadmat([mat_dir, 'test_sparsefloat_7.4_GLNX86.mat']);
assert_isequal(testsparsefloat, testsparsefloat_ref);
%=============================================================================
