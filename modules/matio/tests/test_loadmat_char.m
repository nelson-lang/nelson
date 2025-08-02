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
loadmat([mat_dir, 'test_char_array_one_by_zero.mat']);
assert_isequal(class(var), 'char');
assert_isequal(size(var), [1 0]);
%=============================================================================
clear teststring
loadmat([mat_dir, 'test_char_array_4.2c_SOL2.mat']);
teststring_ref = '"Do nine men interpret?" "Nine men," I nod.';
assert_isequal(teststring, teststring_ref);
%=============================================================================
clear teststring
loadmat([mat_dir, 'test_char_array_6.1_SOL2.mat']);
teststring_ref = '"Do nine men interpret?" "Nine men," I nod.';
assert_isequal(teststring, teststring_ref);
%=============================================================================
clear teststring
loadmat([mat_dir, 'test_char_array_6.5.1_GLNX86.mat']);
teststring_ref = '"Do nine men interpret?" "Nine men," I nod.';
assert_isequal(teststring, teststring_ref);
%=============================================================================
clear teststring
loadmat([mat_dir, 'test_char_array_7.1_GLNX86.mat']);
teststring_ref = '"Do nine men interpret?" "Nine men," I nod.';
assert_isequal(teststring, teststring_ref);
%=============================================================================
clear teststring
loadmat([mat_dir, 'test_char_array_7.4_GLNX86.mat']);
teststring_ref = '"Do nine men interpret?" "Nine men," I nod.';
assert_isequal(teststring, teststring_ref);
%=============================================================================
clear teststringarray
loadmat([mat_dir, 'test_char_array_4.2c_SOL2_1.mat']);
teststringarray_ref = ['one  '; 'two  '; 'three'];
assert_isequal(teststringarray, teststringarray_ref);
%=============================================================================
clear teststringarray
loadmat([mat_dir, 'test_char_array_6.1_SOL2_1.mat']);
teststringarray_ref = ['one  '; 'two  '; 'three'];
assert_isequal(teststringarray, teststringarray_ref);
%=============================================================================
clear teststringarray
loadmat([mat_dir, 'test_char_array_6.5.1_GLNX86_1.mat']);
teststringarray_ref = ['one  '; 'two  '; 'three'];
assert_isequal(teststringarray, teststringarray_ref);
%=============================================================================
clear teststringarray
loadmat([mat_dir, 'test_char_array_7.1_GLNX86_1.mat']);
teststringarray_ref = ['one  '; 'two  '; 'three'];
assert_isequal(teststringarray, teststringarray_ref);
%=============================================================================
clear teststringarray
loadmat([mat_dir, 'test_char_array_7.4_GLNX86_1.mat']);
teststringarray_ref = ['one  '; 'two  '; 'three'];
assert_isequal(teststringarray, teststringarray_ref);
%=============================================================================
testunicode_ref = 'Japanese: 
すべての人間は、生まれながらにして自由であり、
かつ、尊厳と権利と について平等である。
人間は、理性と良心とを授けられており、
互いに同胞の精神をもって行動しなければならない。';
%=============================================================================
clear testunicode
loadmat([mat_dir, 'test_char_array_unicode_7.1_GLNX86.mat']);
assert_isequal(testunicode, testunicode_ref);
%=============================================================================
clear testunicode
loadmat([mat_dir, 'test_char_array_unicode_7.4_GLNX86.mat']);
assert_isequal(testunicode, testunicode_ref);
%=============================================================================
