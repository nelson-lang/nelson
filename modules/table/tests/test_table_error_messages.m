%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
TR = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], 'VariableNames', {'A', 'B'});
%=============================================================================
% a variable number beyond the width
msg = 'Variable index exceeds table dimensions.';
assert_checkerror('T(1, 5)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('T(1, 3)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('T(:, 5)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('T{1, 5}', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('T(1, [true true true])', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('TR(1:2, 3)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('TT(1, 5)', msg, 'Nelson:table:varIndexOutOfRange');
assert_checkerror('TT{1, 5}', msg, 'Nelson:table:varIndexOutOfRange');
%=============================================================================
% a row number beyond the height
msg = 'Row index exceeds table dimensions.';
assert_checkerror('T(3, 1)', msg, 'Nelson:table:rowIndexOutOfRange');
assert_checkerror('T{3, 1}', msg, 'Nelson:table:rowIndexOutOfRange');
%=============================================================================
% an unknown variable name
msg = 'Unrecognized table variable name ''Foo''.';
assert_checkerror('T.Foo', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('x = T.Foo(1);', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('x = T.Foo.Bar;', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('T(:, ''Foo'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('T{:, ''Foo''}', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('T(:, {''A'', ''Foo''})', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('TT.Foo', msg, 'Nelson:timetable:unrecognizedVariable');
assert_checkerror('TT(:, ''Foo'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('TT{:, ''Foo''}', msg, 'Nelson:table:unrecognizedVariable');
%=============================================================================
% an unknown row name
msg = 'Unrecognized table row name ''zz''.';
assert_checkerror('TR(''zz'', :)', msg, 'Nelson:table:notFound');
assert_checkerror('TR{''zz'', 1}', msg, 'Nelson:table:notFound');
%=============================================================================
% an unknown property
msg = 'Unknown table property: Foo.';
assert_checkerror('T.Properties.Foo', msg, 'Nelson:table:unknownProperty');
assert_checkerror('x = T.Properties.Foo;', msg, 'Nelson:table:unknownProperty');
assert_checkerror('x = T.Properties.Foo(1);', msg, 'Nelson:table:unknownProperty');
assert_checkerror('x = T.Properties.Foo.Bar;', msg, 'Nelson:table:unknownProperty');
assert_checkerror('x = subsref(T, substruct(''.'', ''Properties'', ''.'', ''Foo''));', msg, 'Nelson:table:unknownProperty');
assert_checkerror('T.Properties.Foo = 1;', msg, 'Nelson:table:unknownProperty');
assert_checkerror('TT.Properties.Foo', msg, 'Nelson:table:unknownProperty');
assert_checkerror('x = TT.Properties.Foo;', msg, 'Nelson:table:unknownProperty');
assert_checkerror('TT.Properties.Foo = 1;', msg, 'Nelson:table:unknownProperty');
assert_checkerror('T.Properties = struct(''Description'', ''x'', ''Foo'', 1);', msg, 'Nelson:table:unknownProperty');
%=============================================================================
% a structure that is not table properties keeps its message
s = struct('A', 1);
assert_checkerror('s.Foo', 'Reference to non-existent field Foo', 'Nelson:nonExistentField');
%=============================================================================
% functions taking variable names
msg = 'Unrecognized table variable name ''missing''.';
assert_checkerror('sortrows(T, ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('addvars(T, [5; 6], ''Before'', ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('movevars(T, ''A'', ''After'', ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('join(T, T, ''Keys'', ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('join(T, T, ''LeftKeys'', ''missing'', ''RightKeys'', ''A'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('join(T, T, ''LeftKeys'', ''A'', ''RightKeys'', ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('innerjoin(T, T, ''Keys'', ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('stack(T, {''A'', ''missing''})', msg, 'Nelson:table:unrecognizedVariable');
assert_checkerror('varfun(@sum, T, ''InputVariables'', ''missing'')', msg, 'Nelson:table:unrecognizedVariable');
%=============================================================================
% a variable number that is not a positive integer: the same error by
% parentheses (read directly by the interpreter) and by braces
msg = 'index must either be real positive integers or logicals.';
id = 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals';
assert_checkerror('T{1, 1.5}', msg, id);
try
  R = T(1, 1.5);
  err = [];
catch err
end
assert_isequal(err.identifier, id);
assert_isequal(err.message, msg);
try
  R = T(1, -1);
  err = [];
catch err
end
assert_isequal(err.identifier, id);
%=============================================================================
