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
clear teststruct
teststruct_ref = struct();
teststruct_ref.stringfield = 'Rats live on no evil star.';
teststruct_ref.doublefield = [1.4142 2.7183 3.1416];
teststruct_ref.complexfield = [1.4142 + 1.4142i   2.7183 + 2.7183i   3.1416 + 3.1416i];
loadmat([mat_dir, 'test_struct_6.1_SOL2.mat']);
assert_isequal(fieldnames(teststruct), fieldnames(teststruct_ref));
assert_isequal(teststruct.stringfield, teststruct_ref.stringfield);
assert_isapprox(teststruct.doublefield, teststruct_ref.doublefield, 1e-5);
assert_isapprox(teststruct.complexfield, teststruct_ref.complexfield, 1e-5);
%=============================================================================
clear teststruct
loadmat([mat_dir, 'test_struct_6.5.1_GLNX86.mat']);
assert_isequal(fieldnames(teststruct), fieldnames(teststruct_ref));
assert_isequal(teststruct.stringfield, teststruct_ref.stringfield);
assert_isapprox(teststruct.doublefield, teststruct_ref.doublefield, 1e-5);
assert_isapprox(teststruct.complexfield, teststruct_ref.complexfield, 1e-5);
%=============================================================================
clear teststruct
loadmat([mat_dir, 'test_struct_7.1_GLNX86.mat']);
assert_isequal(fieldnames(teststruct), fieldnames(teststruct_ref));
assert_isequal(teststruct.stringfield, teststruct_ref.stringfield);
assert_isapprox(teststruct.doublefield, teststruct_ref.doublefield, 1e-5);
assert_isapprox(teststruct.complexfield, teststruct_ref.complexfield, 1e-5);
%=============================================================================
clear teststruct
loadmat([mat_dir, 'test_struct_7.4_GLNX86.mat']);
assert_isequal(fieldnames(teststruct), fieldnames(teststruct_ref));
assert_isequal(teststruct.stringfield, teststruct_ref.stringfield);
assert_isapprox(teststruct.doublefield, teststruct_ref.doublefield, 1e-5);
assert_isapprox(teststruct.complexfield, teststruct_ref.complexfield, 1e-5);
%=============================================================================
teststructarr_ref = struct();
teststructarr_ref(1).one = 1;
teststructarr_ref(1).two = 2;
teststructarr_ref(2).one = 'number 1';
teststructarr_ref(2).two = 'number 2';
clear teststructarr
loadmat([mat_dir, 'test_struct_array_6.1_SOL2.mat']);
assert_isequal(teststructarr, teststructarr_ref);
%=============================================================================
clear teststructarr
loadmat([mat_dir, 'test_struct_array_6.5.1_GLNX86.mat']);
assert_isequal(teststructarr, teststructarr_ref);
%=============================================================================
clear teststructarr
loadmat([mat_dir, 'test_struct_array_7.1_GLNX86.mat']);
assert_isequal(teststructarr, teststructarr_ref);
%=============================================================================
clear teststructarr
loadmat([mat_dir, 'test_struct_array_7.4_GLNX86.mat']);
assert_isequal(teststructarr, teststructarr_ref);
%=============================================================================
teststructnest_ref = struct();
teststructnest_ref.one = 1;
teststructnest_ref.two = struct('three','number 3');
clear teststructnest
loadmat([mat_dir, 'test_struct_nest_6.1_SOL2.mat']);
assert_isequal(teststructnest, teststructnest_ref);
%=============================================================================
clear teststructnest
loadmat([mat_dir, 'test_struct_nest_6.5.1_GLNX86.mat']);
assert_isequal(teststructnest, teststructnest_ref);
%=============================================================================
clear teststructnest
loadmat([mat_dir, 'test_struct_nest_7.1_GLNX86.mat']);
assert_isequal(teststructnest, teststructnest_ref);
%=============================================================================
clear teststructnest
loadmat([mat_dir, 'test_struct_nest_7.4_GLNX86.mat']);
assert_isequal(teststructnest, teststructnest_ref);
%=============================================================================
clear a
loadmat([mat_dir, 'test_empty_struct.mat']);
ref = struct();
assert_isequal(a, ref);
%=============================================================================
