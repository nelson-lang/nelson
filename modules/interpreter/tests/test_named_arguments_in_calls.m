%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
captureArgs = @(varargin) varargin;
%=============================================================================
A = captureArgs(B = 1);
assert_isequal(A{1}, "B");
assert_isequal(A{2}, 1);
%=============================================================================
args = captureArgs(   B   =    1   );
assert_isequal(args{1}, "B");
assert_isequal(args{2}, 1);
%=============================================================================
args = captureArgs(B = 1, C = 2);
assert_isequal(args{1}, "B");
assert_isequal(args{2}, 1);
assert_isequal(args{3}, "C");
assert_isequal(args{4}, 2);
%=============================================================================
args = captureArgs(42, D = 10);
assert_isequal(args{1}, 42);
assert_isequal(args{2}, "D");
assert_isequal(args{3}, 10);
%=============================================================================
nested = captureArgs(captureArgs(C = 3));
assert_isequal(nested{1}{1}, "C");
assert_isequal(nested{1}{2}, 3);
%=============================================================================
B = 5;
logicArgs = captureArgs(B == 5);
assert_istrue(logicArgs{1});
%=============================================================================
nested = captureArgs(captureArgs(C = 3*2));
assert_isequal(nested{1}{1}, "C");
assert_isequal(nested{1}{2}, 6);
%=============================================================================
