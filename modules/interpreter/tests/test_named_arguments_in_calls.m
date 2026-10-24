%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
captureArgs = @(varargin) varargin;
%=============================================================================
A = captureArgs(B = 1);
asserts.isequal(A{1}, "B");
asserts.isequal(A{2}, 1);
%=============================================================================
args = captureArgs(B   = 1);
asserts.isequal(args{1}, "B");
asserts.isequal(args{2}, 1);
%=============================================================================
args = captureArgs(B = 1, C = 2);
asserts.isequal(args{1}, "B");
asserts.isequal(args{2}, 1);
asserts.isequal(args{3}, "C");
asserts.isequal(args{4}, 2);
%=============================================================================
args = captureArgs(42, D = 10);
asserts.isequal(args{1}, 42);
asserts.isequal(args{2}, "D");
asserts.isequal(args{3}, 10);
%=============================================================================
nested = captureArgs(captureArgs(C = 3));
asserts.isequal(nested{1}{1}, "C");
asserts.isequal(nested{1}{2}, 3);
%=============================================================================
B = 5;
logicArgs = captureArgs(B == 5);
asserts.istrue(logicArgs{1});
%=============================================================================
nested = captureArgs(captureArgs(C = 3 * 2));
asserts.isequal(nested{1}{1}, "C");
asserts.isequal(nested{1}{2}, 6);
%=============================================================================
