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
assert_isequal(nargin('matches'), 2);
assert_isequal(nargout('matches'), 1);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, "Green");
REF = [false, false, false, false];
assert_isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, "green");
REF = [false, true, false, false];
assert_isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, "Green", 'IgnoreCase', true);
REF = [false, true, false, false];
assert_isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, ["yellow","brown"]);
REF = [true, false, false, true];
assert_isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, {'yellow', 'brown'});
REF = [true, false, false, true];
assert_isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
pattern = 'tt';
assert_checkerror('TF = matches(str, pattern, ''error'', ''g'');', _('Wrong value for #3: ''IgnoreCase'' expected.'));
%=============================================================================
str = {'yellow', 'green', 'blue', 'brown'};
R = matches(str, "green");
REF = [false, true, false, false];
assert_isequal(R, REF);
%=============================================================================
str = {'yellow', 'green', 'blue', 'brown'};
R = matches(str, {'green'});
REF = [false, true, false, false];
assert_isequal(R, REF);
%=============================================================================
str = string({'yellow', NaN, 'blue', 'brown'});
R = matches(str, ["green", string(NaN)]);
REF = [false, false, false, false];
assert_isequal(R, REF);
%=============================================================================
str = string({'yellow', NaN, 'blue', 'brown'});
R = matches(str, [string(NaN), "blue"]);
REF = [false, false, true, false];
assert_isequal(R, REF);
%=============================================================================
R = matches(string(NaN), string(NaN));
REF = false;
assert_isequal(R, REF);
%=============================================================================
