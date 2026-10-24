%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('matches'), -3);
asserts.isequal(nargout('matches'), 1);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, "Green");
REF = [false, false, false, false];
asserts.isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, "green");
REF = [false, true, false, false];
asserts.isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, "Green", 'IgnoreCase', true);
REF = [false, true, false, false];
asserts.isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, ["yellow", "brown"]);
REF = [true, false, false, true];
asserts.isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
R = matches(str, {'yellow', 'brown'});
REF = [true, false, false, true];
asserts.isequal(R, REF);
%=============================================================================
str = ["yellow", "green", "blue", "brown"];
pattern = 'tt';
asserts.checkerror('TF = matches(str, pattern, ''error'', ''g'');', _('Wrong value for #3: ''IgnoreCase'' expected.'));
%=============================================================================
str = {'yellow', 'green', 'blue', 'brown'};
R = matches(str, "green");
REF = [false, true, false, false];
asserts.isequal(R, REF);
%=============================================================================
str = {'yellow', 'green', 'blue', 'brown'};
R = matches(str, {'green'});
REF = [false, true, false, false];
asserts.isequal(R, REF);
%=============================================================================
str = string({'yellow', NaN, 'blue', 'brown'});
R = matches(str, ["green", string(NaN)]);
REF = [false, false, false, false];
asserts.isequal(R, REF);
%=============================================================================
str = string({'yellow', NaN, 'blue', 'brown'});
R = matches(str, [string(NaN), "blue"]);
REF = [false, false, true, false];
asserts.isequal(R, REF);
%=============================================================================
R = matches(string(NaN), string(NaN));
REF = false;
asserts.isequal(R, REF);
%=============================================================================
s = ["a", missing, "c"];
R = matches(s, "a");
REF = [true, false, false];
asserts.isequal(R, REF);
%=============================================================================
R = matches({'a'; 'Fire'}, ["WINTER", "Fire"]);
asserts.isequal(R, [false; true]);
%=============================================================================
R = matches(["a"; "Fire"], {'WINTER', 'Fire'});
asserts.isequal(R, [false; true]);
%=============================================================================
R = matches("Fire", {});
asserts.isequal(R, false);
%=============================================================================
R = matches({'a'; 'Fire'}, string([]));
asserts.isequal(R, [false; false]);
%=============================================================================
R = matches({}, "Fire");
asserts.isequal(R, logical(zeros(0, 0)));
%=============================================================================
R = matches(string(missing), {});
asserts.isequal(R, false);
%=============================================================================
R = matches(["a", "Fire"], string(missing));
asserts.isequal(R, [false, false]);
%=============================================================================
R = matches({'a'; 'FIRE'}, ["winter", "fire"], 'IgnoreCase', true);
asserts.isequal(R, [false; true]);
%=============================================================================
R = matches('', "");
asserts.isequal(R, true);
%=============================================================================
