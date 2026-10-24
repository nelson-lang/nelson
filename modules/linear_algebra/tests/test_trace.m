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
asserts.isequal(nargin('trace'), 1);
asserts.isequal(nargout('trace'), 1);
%=============================================================================
asserts.isequal(trace([]), 0);
asserts.isequal(trace(NaN), NaN);
asserts.isequal(trace(Inf), Inf);
asserts.isequal(trace([10, 20; 30, 40]), 50);
asserts.checkerror('trace([10, 20; 30, 40; 50, 60])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('trace([10, 30, 50; 20, 40, 60])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.checkerror('trace([30:100])', sprintf(_('%s expected.'), 'Square matrix'));
asserts.isequal(trace(eye(1000)), 1000);
%=============================================================================
R = trace(sparse([10, 20; 30, 40]));
asserts.isequal(R, 50);
%=============================================================================
R = trace(sparse(single([10, 20; 30, 40])));
asserts.isequal(class(R), 'single');
asserts.isequal(R, single(50));
%=============================================================================
R = trace(sparse(single([1 + 2i 0; 0 3])));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single(4 + 2i), 1e-6);
%=============================================================================
B = [1 2; 3 4];
asserts.isequal(trace(B), 5);
asserts.isequal(trace([]), 0);
%=============================================================================
