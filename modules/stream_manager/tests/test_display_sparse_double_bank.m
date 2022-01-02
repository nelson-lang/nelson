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
format('bank')
%=============================================================================
R = evalc('A = sparse([])');
REF = '
A =

  0×0 empty sparse double matrix

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(ones(0,4))');
REF = '
A =

  0×4 empty sparse double matrix

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(10000,5000)');
REF = '
A =

    All zero sparse: 10000×5000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('sparse([814.1 1 NaN Inf -Inf])');
REF =  '
ans =

    (1,1)           814.10
    (1,2)             1.00
    (1,3)              NaN
    (1,4)              Inf
    (1,5)             -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([81.1 90.2])');
REF =  '
A =

    (1,1)            81.10
    (1,2)            90.20

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(eps)');
REF =  '
A =

    (1,1)             0.00

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(NaN)');
REF = '
A =

    (1,1)              NaN

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(Inf)');
REF =  '
A =

    (1,1)              Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(-Inf)');
REF = '
A =

    (1,1)             -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-3 1.1e-3 1.2e-3])');
REF = '
A =

    (1,1)             0.00
    (1,2)             0.00
    (1,3)             0.00

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-4 1.1e-4 1.2e-4])');
REF =  '
A =

    (1,1)             0.00
    (1,2)             0.00
    (1,3)             0.00

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e4 1.1e4 1.2e4])');
REF = '
A =

    (1,1)         10000.00
    (1,2)         11000.00
    (1,3)         12000.00

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([10 10 13])');
REF =  '
A =

    (1,1)            10.00
    (1,2)            10.00
    (1,3)            13.00

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([2.3, 1e99, 1])');
REF = '
A =

    (1,1)             2.30
    (1,2)   999999999999999967336168804116691273849533185806555472917961779471295845921727862608739868455469056.00
    (1,3)             1.00

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([-1 -20 -4])');
REF =  '
R =

    (1,1)            -1.00
    (1,2)           -20.00
    (1,3)            -4.00

'; 
assert_isequal(R, REF)
%=============================================================================
