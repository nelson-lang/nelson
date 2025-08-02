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
% MATIO 1.5.13 cannot read object type
%=============================================================================
clear testobject
loadmat([mat_dir, 'test_object_6.1_SOL2.mat']);
assert_isequal(lastwarn(), [_('Cannot read variable:'), ' ', 'testobject']);
testobject_ref = struct();
assert_isequal(testobject, testobject_ref);
%=============================================================================
clear testobject
loadmat([mat_dir, 'test_object_6.5.1_GLNX86.mat']);
assert_isequal(lastwarn(), [_('Cannot read variable:'), ' ', 'testobject']);
testobject_ref = struct();
assert_isequal(testobject, testobject_ref);
%=============================================================================
clear testobject
loadmat([mat_dir, 'test_object_7.1_GLNX86.mat']);
assert_isequal(lastwarn(), [_('Cannot read variable:'), ' ', 'testobject']);
testobject_ref = struct();
assert_isequal(testobject, testobject_ref);
%=============================================================================
clear testobject
loadmat([mat_dir, 'test_object_7.4_GLNX86.mat']);
assert_isequal(lastwarn(), [_('Cannot read variable:'), ' ', 'testobject']);
testobject_ref = struct();
assert_isequal(testobject, testobject_ref);
%=============================================================================
