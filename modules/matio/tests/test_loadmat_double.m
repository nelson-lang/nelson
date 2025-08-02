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
testdouble_ref = [0.0000      0.7854      1.5708      2.3562      3.1416      3.9270      4.7124      5.4978      6.2832];
clear testdouble
loadmat([mat_dir, 'test_double_4.2c_SOL2.mat']);
assert_isapprox(testdouble, testdouble_ref, 1e-4)
%=============================================================================
clear testdouble
loadmat([mat_dir, 'test_double_6.1_SOL2.mat']);
assert_isapprox(testdouble, testdouble_ref, 1e-4)
%=============================================================================
clear testdouble
loadmat([mat_dir, 'test_double_6.5.1_GLNX86.mat']);
assert_isapprox(testdouble, testdouble_ref, 1e-4)
%=============================================================================
clear testdouble
loadmat([mat_dir, 'test_double_7.1_GLNX86.mat']);
assert_isapprox(testdouble, testdouble_ref, 1e-4)
%=============================================================================
clear testdouble
loadmat([mat_dir, 'test_double_7.4_GLNX86.mat']);
assert_isapprox(testdouble, testdouble_ref, 1e-4)
%=============================================================================
testcomplex_ref = [1.0000 + 0.0000i     0.7071 + 0.7071i     0.0000 + 1.0000i    -0.7071 + 0.7071i    -1.0000 + 0.0000i    -0.7071 - 0.7071i    -0.0000 - 1.0000i     0.7071 - 0.7071i     1.0000 - 0.0000i];
clear testcomplex
loadmat([mat_dir, 'test_complex_4.2c_SOL2.mat']);
assert_isapprox(testcomplex, testcomplex_ref, 1e-4)
%=============================================================================
clear testcomplex
loadmat([mat_dir, 'test_complex_6.1_SOL2.mat']);
assert_isapprox(testcomplex, testcomplex_ref, 1e-4)
%=============================================================================
clear testcomplex
loadmat([mat_dir, 'test_complex_6.5.1_GLNX86.mat']);
assert_isapprox(testcomplex, testcomplex_ref, 1e-4)
%=============================================================================
clear testcomplex
loadmat([mat_dir, 'test_complex_7.1_GLNX86.mat']);
assert_isapprox(testcomplex, testcomplex_ref, 1e-4)
%=============================================================================
clear testcomplex
loadmat([mat_dir, 'test_complex_7.4_GLNX86.mat']);
assert_isapprox(testcomplex, testcomplex_ref, 1e-4)
%=============================================================================
testmatrix_ref = [1     2     3     4     5;
2     0     0     0     0;
3     0     0     0     0];
clear testmatrix
loadmat([mat_dir, 'test_matrix_4.2c_SOL2.mat']);
assert_isequal(testmatrix, testmatrix_ref)
%=============================================================================
clear testmatrix
loadmat([mat_dir, 'test_matrix_6.1_SOL2.mat']);
assert_isequal(testmatrix, testmatrix_ref)
%=============================================================================
clear testmatrix
loadmat([mat_dir, 'test_matrix_6.5.1_GLNX86.mat']);
assert_isequal(testmatrix, testmatrix_ref)
%=============================================================================
clear testmatrix
loadmat([mat_dir, 'test_matrix_7.1_GLNX86.mat']);
assert_isequal(testmatrix, testmatrix_ref)
%=============================================================================
clear testmatrix
loadmat([mat_dir, 'test_matrix_7.4_GLNX86.mat']);
assert_isequal(testmatrix, testmatrix_ref)
%=============================================================================
test3dmatrix_ref = reshape(1:24,2,3,4);
clear test3dmatrix
loadmat([mat_dir, 'test_3dmatrix_6.1_SOL2.mat']);
assert_isequal(test3dmatrix, test3dmatrix_ref)
%=============================================================================
clear test3dmatrix
loadmat([mat_dir, 'test_3dmatrix_6.5.1_GLNX86.mat']);
assert_isequal(test3dmatrix, test3dmatrix_ref)
%=============================================================================
clear test3dmatrix
loadmat([mat_dir, 'test_3dmatrix_7.1_GLNX86.mat']);
assert_isequal(test3dmatrix, test3dmatrix_ref)
%=============================================================================
clear test3dmatrix
loadmat([mat_dir, 'test_3dmatrix_7.4_GLNX86.mat']);
assert_isequal(test3dmatrix, test3dmatrix_ref)
%=============================================================================
