%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = jsonencode(char(13));
ref = '"\r"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(char(10));
ref = '"\n"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode('"');
ref = '"\""';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode('\r');
ref = '"\\r"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode('\n');
ref = '"\\n"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode('\\');
ref = '"\\\\"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(ones(3, 0));
ref = '[]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode({});
ref = '[]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode([]);
ref = '[]';
asserts.isequal(r, ref);
%=============================================================================
field1 = 'f1'; value1 = zeros(1, 10);
field2 = 'f2'; value2 = {'a', 'b'};
field3 = 'f3'; value3 = {pi, pi * pi};
field4 = 'f4'; value4 = {'fourth'};
s = struct(field1, value1, field2, value2, field3, value3, field4, value4);
r = jsonencode(s);
ref = '[{"f1":[0,0,0,0,0,0,0,0,0,0],"f2":"a","f3":3.141592653589793,"f4":"fourth"},{"f1":[0,0,0,0,0,0,0,0,0,0],"f2":"b","f3":9.869604401089358,"f4":"fourth"}]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(['Nel'; 'Son']);
ref = '["Nel","Son"]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode([1, 2, 3; 4, 5, 6; 7, 8, 9]);
ref = '[[1,2,3],[4,5,6],[7,8,9]]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(char(20));
ref = '"\u0014"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(reshape(char([22, 20, 32; 42, 52, 62; 72, 82, 92]), 1, 3, 3));
ref = '["\u0016*H","\u00144R"," >\\"]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(reshape([1, 2, 3; 4, 5, 6; 7, 8, 9], 1, 3, 3));
ref = '[[1,2,3],[4,5,6],[7,8,9]]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(char([22, 20, 32; 42, 52, 62; 72, 82, 92]));
ref = '["\u0016\u0014 ","*4>","HR\\"]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(true);
ref = 'true';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode([true, false, false]);
ref = '[true,false,false]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(logical([1, 0, 0, 1, 1, 0]));
ref = '[true,false,false,true,true,false]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode('漢字');
ref = '"漢字"';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode('');
ref = '""';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(2.6);
ref = '2.6';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(1:3);
ref = '[1,2,3]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(eye(4));
ref = '[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode({'B', true, [22; 32]});
ref = '["B",true,[22,32]]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(struct('a', 'value'));
ref = '{"a":"value"}';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode(struct('aa', {true, true, false, false}));
ref = '[{"aa":true},{"aa":true},{"aa":false},{"aa":false}]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode([1, 2, NaN, 3, Inf, -Inf]);
ref = '[1,2,null,3,null,null]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode([1, 2, NaN, 3, Inf, -Inf], 'ConvertInfAndNaN', true);
ref = '[1,2,null,3,null,null]';
asserts.isequal(r, ref);
%=============================================================================
r = jsonencode([1, 2, NaN, 3, Inf, -Inf], 'ConvertInfAndNaN', false);
ref = '[1,2,NaN,3,Inf,-Inf]';
asserts.isequal(r, ref);
%=============================================================================
% PrettyPrint option formats with two-space indentation and a space after colons
R = jsonencode(struct('a', 1), 'PrettyPrint', true);
REF = sprintf('{\n  "a": 1\n}');
assert_isequal(R, REF);
%=============================================================================
R = jsonencode([1 2 3], 'PrettyPrint', true);
REF = sprintf('[\n  1,\n  2,\n  3\n]');
assert_isequal(R, REF);
%=============================================================================
% empty collections stay inline
R = jsonencode(struct('empty', [], 'nested', struct('x', 1)), 'PrettyPrint', true);
REF = sprintf('{\n  "empty": [],\n  "nested": {\n    "x": 1\n  }\n}');
assert_isequal(R, REF);
%=============================================================================
% PrettyPrint combines with ConvertInfAndNaN (true converts NaN to null)
R = jsonencode(struct('a', NaN), 'ConvertInfAndNaN', true, 'PrettyPrint', true);
REF = sprintf('{\n  "a": null\n}');
assert_isequal(R, REF);
% ConvertInfAndNaN false keeps the NaN literal under PrettyPrint
R = jsonencode(struct('a', NaN), 'ConvertInfAndNaN', false, 'PrettyPrint', true);
REF = sprintf('{\n  "a": NaN\n}');
assert_isequal(R, REF);
%=============================================================================
% PrettyPrint false keeps the compact form
assert_isequal(jsonencode(struct('a', 1), 'PrettyPrint', false), '{"a":1}');
%=============================================================================
% an unknown option name errors
assert_checkerror('jsonencode(1, ''BadName'', true)', sprintf(_('%s expected.'), 'Wrong value for argument: ''ConvertInfAndNaN'' or ''PrettyPrint'''));
assert_checkerror('jsonencode(1, ''BadName'', true)', sprintf(_('%s expected.'), 'Wrong value for argument: ''ConvertInfAndNaN'' or ''PrettyPrint'''), 'Nelson:jsonencode:invalidOption');
%=============================================================================
