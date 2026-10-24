%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], [5; 6]);
T = addprop(T, {'Foo', 'Bar'}, {'variable', 'table'});
T.Properties.CustomProperties.Foo = ["p", "q", "r"];
T.Properties.CustomProperties.Bar = 42;
%=============================================================================
% deleting variables deletes their per-variable custom property elements
R = removevars(T, 2);
asserts.isequal(R.Properties.CustomProperties.Foo, ["p", "r"]);
asserts.isequal(R.Properties.CustomProperties.Bar, 42);
R = T;
R.Var2 = [];
asserts.isequal(R.Properties.CustomProperties.Foo, ["p", "r"]);
R = T;
R(:, [1 3]) = [];
asserts.isequal(R.Properties.CustomProperties.Foo, "q");
R = removevars(removevars(T, 1), 1);
asserts.isequal(R.Properties.CustomProperties.Foo, "r");
%=============================================================================
% selecting and moving variables reorders them
R = T(:, [3 1]);
asserts.isequal(R.Properties.CustomProperties.Foo, ["r", "p"]);
R = T(:, [3 1 1]);
asserts.isequal(R.Properties.CustomProperties.Foo, ["r", "p", "p"]);
R = movevars(T, 'Var1', 'After', 'Var3');
asserts.isequal(R.Properties.CustomProperties.Foo, ["q", "r", "p"]);
asserts.isequal(R.Properties.CustomProperties.Bar, 42);
%=============================================================================
% a table property is kept as it is
X = rmprop(T, 'Foo');
X = addprop(X, 'Foo', 'table');
X.Properties.CustomProperties.Foo = [1 2];
X = removevars(X, 1);
asserts.isequal(X.Properties.CustomProperties.Foo, [1 2]);
%=============================================================================
% an unset per-variable property stays unset
E = table([1; 2], [3; 4]);
E = addprop(E, 'Foo', 'variable');
E = removevars(E, 1);
asserts.isequal(E.Properties.CustomProperties.Foo, []);
%=============================================================================
% one element per variable, stored as a row, or 0x0
C = T;
C.Properties.CustomProperties.Foo = [1; 2; 3];
asserts.isequal(C.Properties.CustomProperties.Foo, [1 2 3]);
C.Properties.CustomProperties.Foo = {};
asserts.isequal(C.Properties.CustomProperties.Foo, {});
msg = 'The custom property ''Foo'' must be a vector with one element for each variable in the table or must be a 0x0 empty (e.g. [], {}).';
assert_checkerror('C.Properties.CustomProperties.Foo = ["a", "b"];', msg, 'Nelson:table:IncorrectNumberOfVarCustomProps');
assert_checkerror('C.Properties.CustomProperties.Foo = ones(2, 3);', msg, 'Nelson:table:IncorrectNumberOfVarCustomProps');
C.Properties.CustomProperties.Bar = [1 2];
asserts.isequal(C.Properties.CustomProperties.Bar, [1 2]);
%=============================================================================
% the property type survives a struct round trip and a timetable
S = table.fromStruct(struct(T));
S = removevars(S, 3);
asserts.isequal(S.Properties.CustomProperties.Foo, ["p", "q"]);
TT = table2timetable(T, 'RowTimes', seconds([1; 2]));
TR = removevars(TT, 2);
asserts.isequal(TR.Properties.CustomProperties.Foo, ["p", "r"]);
TR = TT;
TR(:, 1) = [];
asserts.isequal(TR.Properties.CustomProperties.Foo, ["q", "r"]);
assert_checkerror('TT.Properties.CustomProperties.Foo = ["a", "b"];', msg, 'Nelson:table:IncorrectNumberOfVarCustomProps');
%=============================================================================
