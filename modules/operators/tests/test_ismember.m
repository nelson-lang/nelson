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
assert_isequal(nargin('ismember'), 2)
assert_isequal(nargout('ismember'), 1)
%=============================================================================
R = ismember({''}, {'abc', 'def'});
assert_isfalse(R)
%=============================================================================
R = ismember('abc', {'abc', 'def'});
assert_istrue(R)
%=============================================================================
R = ismember([], [1, 2]);
assert_isequal(R, logical([]))
%=============================================================================
R = ismember({}, {'a', 'b'});
assert_isequal(R, logical([]))
%=============================================================================
R = ismember([], 'a');
assert_isequal(R, logical([]))
%=============================================================================
R = ismember('', {'abc', 'def'});
assert_isfalse(R)
%=============================================================================
R = ismember(1, 'abc');
assert_isfalse(R)
%=============================================================================
R = ismember('abc', 1);
assert_isequal(R, [false false false]);
%=============================================================================
R = ismember('abc', 99);
assert_isequal(R, [false false true]);
%=============================================================================
assert_checkerror('R = ismember([], {1, 2})', _('function double_ismember undefined.'))
%=============================================================================
assert_checkerror('R = ismember({[]}, {1, 2})', _('function cell_ismember undefined.'))
%=============================================================================
assert_checkerror('R = ismember({}, {1, 2})', _('function cell_ismember undefined.'))
%=============================================================================
assert_checkerror('R = ismember({1}, {''1'', ''2''})', _('function cell_ismember undefined.'))
%=============================================================================
R = ismember({'1'}, {'1' '2'});
assert_istrue(R)
%=============================================================================
R = ismember([1 2 3], [5 4 3 1]);
assert_isequal(R, [true false true])
%=============================================================================
R = ismember({'foo', 'bar'}, {'foobar'});
assert_isequal(R, [false false])
%=============================================================================
R = ismember({'foo'}, {'foobar'});
assert_isfalse(R)
%=============================================================================
R = ismember({'bar'}, {'foobar'});
assert_isfalse(R)
%=============================================================================
R = ismember({'bar'}, {'foobar', 'bar'});
assert_istrue(R)
%=============================================================================
R = ismember({'foo', 'bar'}, {'foobar', 'bar'});
assert_isequal(R, [false true])
%=============================================================================
R = ismember({'xfb', 'f', 'b'}, {'fb', 'b'});
assert_isequal(R, [false false true])
%=============================================================================
R = ismember('1', '0123456789.');
assert_istrue(R)
%=============================================================================
R = ismember("1", "0123456789.");
assert_isfalse(R)
%=============================================================================
R = ismember("0123456789.", "1");
assert_isfalse(R)
%=============================================================================
R = ismember('0123456789.', '1');
assert_isequal(R, logical([0   1   0   0   0   0   0   0   0   0   0]));
%=============================================================================
