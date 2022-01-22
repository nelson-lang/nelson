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
format('short')
%=============================================================================
R = evalc('A = {1}');
REF =  '
A =

  1×1 cell array

    {[1]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {pi}');
REF =  '
A =

  1×1 cell array

    {[3.1416]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {NaN}');
REF = '
A =

  1×1 cell array

    {[NaN]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-Inf}');
REF =  '
A =

  1×1 cell array

    {[-Inf]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {Inf}');
REF =  '
A =

  1×1 cell array

    {[Inf]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(pi, pi)}');
REF =  '
A =

  1×1 cell array

    {[3.1416 + 3.1416i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1, 0)}');
REF = '
A =

  1×1 cell array

    {[1.0000 + 0.0000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1, NaN)}');
REF = '
A =

  1×1 cell array

    {[1.0000 +    NaNi]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(NaN, NaN)}');
REF = '
A =

  1×1 cell array

    {[NaN +    NaNi]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(NaN, -Inf)}');
REF = '
A =

  1×1 cell array

    {[NaN -    Infi]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(0, 0)}');
REF = '
A =

  1×1 cell array

    {[0.0000 + 0.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(3, eps)}');
REF = '
A =

  1×1 cell array

    {[3.0000 + 0.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(eps, 3)}');
REF = '
A =

  1×1 cell array

    {[0.0000 + 3.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(3, 1e-9)}');
REF =  '
A =

  1×1 cell array

    {[3.0000 + 0.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(1e-9, 3)}');
REF = '
A =

  1×1 cell array

    {[0.0000 + 3.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(3, 1e9)}');
REF = '
A =

  1×1 cell array

    {[3.0000e+00 + 1.0000e+09i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(1e9, 3)}');
REF = '
A =

  1×1 cell array

    {[1.0000e+09 + 3.0000e+00i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(3, 100)}');
REF =  '
A =

  1×1 cell array

    {[3.0000e+00 + 1.0000e+02i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(100, 3)}');
REF = '
A =

  1×1 cell array

    {[1.0000e+02 + 3.0000e+00i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(3, 99)}');
REF = '
A =

  1×1 cell array

    {[3.0000 +99.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(99, 3)}');
REF = '
A =

  1×1 cell array

    {[99.0000 + 3.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(3, 0.001)}');
REF = '
A =

  1×1 cell array

    {[3.0000 + 0.0010i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(0.001, 3)}');
REF =  '
A =

  1×1 cell array

    {[0.0010 + 3.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(0.99999, eps)}');
REF = '
A =

  1×1 cell array

    {[1.0000 + 0.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(eps, 0.99999)}');
REF = '
A =

  1×1 cell array

    {[0.0000 + 1.0000i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(0.00099999, eps)}');
REF = '
A =

  1×1 cell array

    {[9.9999e-04 + 2.2204e-16i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(eps, 0.00099999)}');
REF = '
A =

  1×1 cell array

    {[2.2204e-16 + 9.9999e-04i]}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = {complex(1.8e99, -eps)}');
REF = '
A =

  1×1 cell array

    {[1.8000e+99 - 2.2204e-16i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1.8e99, 0)}');
REF = '
A =

  1×1 cell array

    {[1.8000e+99 + 0.0000e+00i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1, -eps)}');
REF = '
A =

  1×1 cell array

    {[1.0000 - 0.0000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(10, -eps)}');
REF =     '
A =

  1×1 cell array

    {[10.0000 - 0.0000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(100, -eps)}');
REF = '
A =

  1×1 cell array

    {[1.0000e+02 - 2.2204e-16i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1000, -eps)}');
REF =  '
A =

  1×1 cell array

    {[1.0000e+03 - 2.2204e-16i]}

';
assert_isequal(R, REF)
%=============================================================================
