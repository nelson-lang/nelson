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
assert_isequal(nargin('base2dec'), 2)
assert_isequal(nargout('base2dec'), 1)
%=============================================================================
R = base2dec('212', 3);
REF = 23;
assert_isequal(R, REF);
%=============================================================================
R = base2dec('', 3);
REF = [];
assert_isequal(R, REF);
%=============================================================================
R = base2dec('11120', 3);
REF = 123;
assert_isequal(R, REF);
%=============================================================================
cmd = 'base2dec(''-14'', 2);';
assert_checkerror(cmd, [_('Cannot convert: '), '-14']);
%=============================================================================
R = base2dec({'A2', '42A'}, 16);
REF =  [162; 1066];
assert_isequal(R, REF);
%=============================================================================
R = base2dec('WIKIPEDIA', 36);
REF = 9.173073869129800e+13;
assert_isequal(R, REF);
%=============================================================================
M = ['01010101'; '10101010'; ' 101010 '];
R = base2dec(M, 2);
REF = [85; 170; 42];
assert_isequal(R, REF);
%=============================================================================
assert_isequal(1,		      base2dec('1', 36));
assert_isequal(10,		      base2dec('A', 36));
assert_isequal(100,		      base2dec('2S', 36));
assert_isequal(1000,		  base2dec('RS', 36));
assert_isequal(10000,	      base2dec('7PS', 36));
assert_isequal(100000,		  base2dec('255S', 36));
assert_isequal(1000000,		  base2dec('LFLS', 36));
assert_isequal(1000000000,	  base2dec('GJDGXS', 36));
assert_isequal(1000000000000, base2dec('CRE66I9S', 36));
%=============================================================================
assert_isequal(base2dec(' 1 1 1 ', 2), 7);
%=============================================================================
