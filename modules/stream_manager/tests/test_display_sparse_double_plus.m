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
format('+')
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
R = evalc('A = sparse(eps)');
REF = '
A =

    (1,1)   +

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(NaN)');
REF = '
A =

    (1,1)   +

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(Inf)');
REF = '
A =

    (1,1)   +

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(-Inf)');
REF = '
A =

    (1,1)   -

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-3 -1.1e-3 1.2e-3])');
REF = '
A =

    (1,1)   +
    (1,2)   -
    (1,3)   +

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([10 -10 13])');
REF = '
A =

    (1,1)   +
    (1,2)   -
    (1,3)   +

';
assert_isequal(R, REF)
%=============================================================================
A = zeros(4000, 4000);
A(3, 3) = 1e-3;
A(1000, 1000) = 1;
R = evalc('R = sparse(A)');
REF =  '
R =

    (3,3)         +
    (1000,1000)   +

';
assert_isequal(R, REF)
%=============================================================================
