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
format('hex')
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
R = evalc('R = sparse([814.1 1 NaN Inf -Inf])');
REF = '
R =

    (1,1)      408970cccccccccd
    (1,2)      3ff0000000000000
    (1,3)      fff8000000000000
    (1,4)      7ff0000000000000
    (1,5)      fff0000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([81.1 90.2])');
REF = '
A =

    (1,1)      4054466666666666
    (1,2)      40568ccccccccccd

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(eps)');
REF = '
A =

    (1,1)      3cb0000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(NaN)');
REF = '
A =

    (1,1)      fff8000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(Inf)');
REF =  '
A =

    (1,1)      7ff0000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(-Inf)');
REF = '
A =

    (1,1)      fff0000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-3 1.1e-3 1.2e-3])');
REF = '
A =

    (1,1)      3f50624dd2f1a9fc
    (1,2)      3f5205bc01a36e2f
    (1,3)      3f53a92a30553261

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-4 1.1e-4 1.2e-4])');
REF = '
A =

    (1,1)      3f1a36e2eb1c432d
    (1,2)      3f1cd5f99c38b04b
    (1,3)      3f1f75104d551d69

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e4 1.1e4 1.2e4])');
REF = '
A =

    (1,1)      40c3880000000000
    (1,2)      40c57c0000000000
    (1,3)      40c7700000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([10 10 13])');
REF = '
A =

    (1,1)      4024000000000000
    (1,2)      4024000000000000
    (1,3)      402a000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([2.3, 1e99, 1])');
REF = '
A =

    (1,1)      4002666666666666
    (1,2)      547d42aea2879f2e
    (1,3)      3ff0000000000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([-1 -20 -4])');
REF =  '
R =

    (1,1)      bff0000000000000
    (1,2)      c034000000000000
    (1,3)      c010000000000000

'; 
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([814.1 1 NaN Inf -Inf])');
REF = '
R =

    (1,1)      408970cccccccccd
    (1,2)      3ff0000000000000
    (1,3)      fff8000000000000
    (1,4)      7ff0000000000000
    (1,5)      fff0000000000000

';
assert_isequal(R, REF)
%=============================================================================
