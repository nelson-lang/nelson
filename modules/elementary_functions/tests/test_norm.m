%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('norm'), 1)
assert_isequal(nargout('norm'), 1)
%=============================================================================
M = [10, 20, 30, -40];
R = norm(M, 1);
REF = 100;
assert_isequal(R, REF);
%=============================================================================
assert_isequal(R, REF);
M = [10, 20; 30, -40];
R = norm(M, 1);
REF = 60;
assert_isequal(R, REF);
%=============================================================================
M = [-6.6613d-16+2.7756d-17i, -7.7716d-16+5.5511d-17i;
-1.5543d-15+1.1102d-16i, -2.6645d-15-2.7756d-17i];
R = norm(M);
REF = 3.2477e-15;
assert_isapprox(R, REF, 1e-6);
%=============================================================================
M = [];
R = norm(M);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
M = [];
R = norm(M, 1);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
M = [];
R = norm(M, 2);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
M = [];
R = norm(M, Inf);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
M = [];
R = norm(M, 'fro');
REF = 0;
assert_isequal(R, REF);
%=============================================================================
M = Inf;
R = norm(M);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = Inf;
R = norm(M, 1);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = Inf;
R = norm(M, 2);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = Inf;
R = norm(M, Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = Inf;
R = norm(M, 'fro');
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 3];
R = norm(M);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 3];
R = norm(M, 1);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 3];
R = norm(M, 2);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 3];
R = norm(M, Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 3];
R = norm(M, 'fro');
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 NaN];
R = norm(M);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 NaN];
R = norm(M, 1);
REF = 3;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 NaN];
R = norm(M, 2);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 NaN];
R = norm(M, Inf);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
M = [1, Inf; 2 NaN];
R = norm(M, 'fro');
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
M = complex(0, 1);
R = norm(M);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
M = complex(0, 1);
R = norm(M, 1);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
M = complex(0, 1);
R = norm(M, 2);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
M = complex(0, 1);
R = norm(M, Inf);
REF = 1;
assert_isequal(R, REF);
%=============================================================================
M = complex(0, 1);
R = norm(M, 'fro');
REF = 1;
assert_isequal(R, REF);
%=============================================================================
M = [0, 0, 0, 0;
1, -0.18182, 0, -0.45455;
0, 2.6727, 0, 31.182;
0, 0, 1, 0];
R = norm(M);
REF =  31.2998;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
M = [0, 0, 0, 0;
1, -0.18182, 0, -0.45455;
0, 2.6727, 0, 31.182;
0, 0, 1, 0];
R = norm(M, 1);
REF =  31.6365;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
M = [0, 0, 0, 0;
1, -0.18182, 0, -0.45455;
0, 2.6727, 0, 31.182;
0, 0, 1, 0];
R = norm(M, Inf);
REF = 33.8547;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
M = [0, 0, 0, 0;
1, -0.18182, 0, -0.45455;
0, 2.6727, 0, 31.182;
0, 0, 1, 0];
R = norm(M, 'fro');
REF = 31.3321;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
N = [1 2 3; 4 5 6];
M = N + i*N;
R = norm(M,'fro');
REF = 13.4907;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
M = ones(10, 10);
R = norm(M, 1);
REF = 10;
assert_isequal(R, REF);
%=============================================================================
M = ones(10, 10);
R = norm(M, 2);
REF = 10;
assert_isequal(R, REF);
%=============================================================================
M = ones(10, 10);
R = norm(M, Inf);
REF = 10;
assert_isequal(R, REF);
%=============================================================================
M = ones(10, 10);
R = norm(M, 'fro');
REF = 10;
assert_isequal(R, REF);
%=============================================================================
V = [10 ,20 , 30, -10, -20, -30];
R = norm(V, 0);
REF = Inf;
%=============================================================================
V = [10 ,20 , 30, -10, -20, -30];
R = norm(V, NaN);
REF = NaN;
%=============================================================================
V = [1,2,3,-4];
R = norm(V, 2.5);
REF = 4.9402;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
V = [30 20];
R = norm(V, -1);
REF = 12;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
M = [1, 2; 3, -4];
assert_checkerror('R = norm(M, -Inf)', _('Wrong value for #2 argument.'));
%=============================================================================
M = [1, 2; 3, -4];
assert_checkerror('R = norm(M, 1.5)', _('Wrong value for #2 argument.'));
%=============================================================================
