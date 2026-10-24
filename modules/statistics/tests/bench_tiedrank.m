%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
X = randi(1000, 50000, 8);
tic();
[R, tieadj] = tiedrank(X);
t = toc();
asserts.isequal(size(R), size(X));
asserts.isequal(size(tieadj), [1 8]);
asserts.istrue(t >= 0);
%=============================================================================
Y = randi(100, 5000, 20);
tic();
[R, tieadj] = tiedrank(Y, 1);
t = toc();
asserts.isequal(size(R), size(Y));
asserts.isequal(size(tieadj), [3 20]);
asserts.istrue(t >= 0);
%=============================================================================
