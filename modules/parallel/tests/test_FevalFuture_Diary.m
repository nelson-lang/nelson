%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([nelsonroot(), '/modules/parallel/tests/functions'])
fptr = str2func('func_diary');
f = parfeval(backgroundPool, fptr, 0, 5, 1000);
R = f.Diary;
assert_isequal(R, '')
%=============================================================================
sleep(6);
R = f.Diary;
assert_istrue(length(R) >= 8);
%=============================================================================
sleep(6);
R = f.Diary;
assert_istrue(length(R) >= 16);
%=============================================================================
