%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('log2'), 1)
assert_isequal(nargout('log2'), 2)
%=============================================================================
V = [1 pi -3 eps];
R = log2(V);
REF = [0.0000 + 0.0000i   1.6515 + 0.0000i   1.5850 + 4.5324i -52.0000 + 0.0000i];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
V = [1 pi -3 eps];
[F, E] = log2(V);
REF_F = [0.5000    0.7854   -0.7500    0.5000];
REF_E = [1     2     2   -51];
assert_isapprox(F, REF_F, 1e-4);
assert_isapprox(E, REF_E, 1e-4);
%=============================================================================
R = log2([]);
REF = [];
assert_isequal(R, REF);
%=============================================================================
R = log2(0);
REF = -Inf;
assert_isequal(R, REF);
%=============================================================================
R = log2(1);
REF = 0;
assert_isequal(R, REF);
%=============================================================================
R = log2(NaN);
REF = NaN;
assert_isequal(R, REF);
%=============================================================================
R = log2(Inf);
REF = Inf;
assert_isequal(R, REF);
%=============================================================================
R = log2(-Inf);
REF = Inf + 4.5324i;
%=============================================================================
[F, E] = log2([]);
REF_F = [];
assert_isequal(F, REF_F);
REF_E = [];
assert_isequal(E, REF_E);
%=============================================================================
[F, E] = log2(0);
REF_F = 0;
assert_isequal(F, REF_F);
REF_E = 0;
assert_isequal(E, REF_E);
%=============================================================================
[F, E] = log2(1);
REF_F = 0.5;
REF_E = 1;
assert_isequal(F, REF_F);
assert_isequal(E, REF_E);
%=============================================================================
[F, E] = log2(NaN);
REF_F = NaN;
REF_E = 0;
assert_isequal(F, REF_F);
assert_isequal(E, REF_E);
%=============================================================================
[F, E] = log2(Inf);
REF_F = Inf;
REF_E = 0;
assert_isequal(F, REF_F);
assert_isequal(E, REF_E);
%=============================================================================
[F, E] = log2(-Inf);
REF_F = -Inf;
REF_E = 0;
assert_isequal(F, REF_F);
assert_isequal(E, REF_E);
%=============================================================================
V = [0 1 2 10 Inf NaN];
R = log2(V);
REF = [ -Inf         0    1.0000    3.3219       Inf       NaN];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
V = [0 1 2 10 Inf NaN];
[F, E] = log2(V);
REF_F = [0    0.5000    0.5000    0.6250       Inf       NaN];
REF_E = [0     1     2     4     0     0];
assert_isapprox(F, REF_F, 1e-4);
assert_isapprox(E, REF_E, 1e-4);
%=============================================================================
M = [1, 2, complex(9,8), complex(2,3); complex(2,3), complex(0,1), 0 , 6];
R = log2(M);
REF = [   0.0000+0.0000i,   1.0000+0.0000i,   3.5900+1.0483i,   1.8502+1.4179i;
1.8502+1.4179i,   0.0000+2.2662i,     -Inf+0.0000i,   2.5850+0.0000i];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
M = [1, 2, complex(9,8), complex(2,3); complex(2,3), complex(0,1), 0 , 6];
[F, E] = log2(M);
REF_F = [    0.5000    0.5000    0.5625    0.5000;
0.5000         0         0    0.7500];
REF_E = [1     2     4     2;
     2     0     0     3];
assert_isapprox(F, REF_F, 1e-4);
assert_isapprox(E, REF_E, 1e-4);
%=============================================================================
C = complex(3.99, 3.99);
R = log2(C);
REF = 2.4964 + 1.1331i;
assert_isapprox(R, REF, 1e-4);
%=============================================================================
C = complex(3.99, 3.99);
[F, E] = log2(C);
REF_F = 0.9975;
REF_E = 2;
assert_isapprox(F, REF_F, 1e-4);
assert_isapprox(E, REF_E, 1e-4);
%=============================================================================
