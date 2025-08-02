%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
