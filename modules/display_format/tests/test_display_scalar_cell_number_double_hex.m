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
R = evalc('A = {1}');
REF =    '
A =

  1×1 cell array

    {[3ff0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {pi}');
REF =  '
A =

  1×1 cell array

    {[400921fb54442d18]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-pi}');
REF =  '
A =

  1×1 cell array

    {[c00921fb54442d18]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {NaN}');
REF = '
A =

  1×1 cell array

    {[fff8000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-Inf}');
REF =  '
A =

  1×1 cell array

    {[fff0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {Inf}');
REF =  '
A =

  1×1 cell array

    {[7ff0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {eps}');
REF = '
A =

  1×1 cell array

    {[3cb0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {-eps}');
REF =   '
A =

  1×1 cell array

    {[bcb0000000000000]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(pi, pi)}');
REF =  '
A =

  1×1 cell array

    {[400921fb54442d18   400921fb54442d18i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1, 0)}');
REF =  '
A =

  1×1 cell array

    {[3ff0000000000000   0000000000000000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(NaN, NaN)}');
REF =  '
A =

  1×1 cell array

    {[fff8000000000000   fff8000000000000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(1.8e99, -eps)}');
REF =  '
A =

  1×1 cell array

    {[548a559d2be075aa   bcb0000000000000i]}

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = {complex(ones(2,2), -eps)}');
REF =  '
A =

  1×1 cell array

    {2×2 double}

';
assert_isequal(R, REF)
%=============================================================================
