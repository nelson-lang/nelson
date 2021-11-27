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
A = complex(9, 3);
R = evalc('A');
REF =  '
A =

   9.0000 + 3.0000i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(9, NaN);
R = evalc('A');
REF =  '
A =

   9.0000 +    NaNi

';
assert_isequal(R, REF)
%=============================================================================
A = complex(NaN, NaN);
R = evalc('A');
REF =  '
A =

      NaN +    NaNi

';
assert_isequal(R, REF)
%=============================================================================
A = complex(NaN, 3);
R = evalc('A');
REF =  '
A =

      NaN + 3.0000i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(100, 3);
R = evalc('A');
REF =  '
A =

   1.0000e+02 + 3.0000e+00i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(99, 3);
R = evalc('A');
REF =      '
A =

  99.0000 + 3.0000i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(3, 100);
R = evalc('A');
REF =  '
A =

   3.0000e+00 + 1.0000e+02i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(3, 99);
R = evalc('A');
REF =  '
A =

   3.0000 +99.0000i

';
assert_isequal(R, REF)
%=============================================================================
