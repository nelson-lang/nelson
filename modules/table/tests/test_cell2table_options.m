%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
C = {5, 'a'; 12, 'b'};
T = cell2table(C, 'RowNames', {'r1'; 'r2'});
asserts.isequal(T.Properties.RowNames, {'r1'; 'r2'});
asserts.isequal(T.Properties.VariableNames, {'C1', 'C2'});
asserts.isequal(T.C1, [5; 12]);
asserts.isequal(T.C2, {'a'; 'b'});
%=============================================================================
T = cell2table(C, 'rownames', ["r1", "r2"]);
asserts.isequal(T.Properties.RowNames, {'r1'; 'r2'});
%=============================================================================
T = cell2table(C, 'RowNames', {'r1', 'r2'}, 'variablenames', ["x", "y"]);
asserts.isequal(T.Properties.RowNames, {'r1'; 'r2'});
asserts.isequal(T.Properties.VariableNames, {'x', 'y'});
asserts.isequal(T.x, [5; 12]);
asserts.isequal(T.y, {'a'; 'b'});
%=============================================================================
T = cell2table(C, "DimensionNames", {'Obs', 'Data'});
asserts.isequal(T.Properties.DimensionNames, {'Obs', 'Data'});
%=============================================================================
T = cell2table({1, 2; 3, 4});
asserts.isequal(T.Properties.VariableNames, {'Var1', 'Var2'});
asserts.isequal(T.Var2, [2; 4]);
%=============================================================================
E = cell(0, 2);
T = cell2table(E);
asserts.isequal(size(T), [0 2]);
asserts.isequal(T.Properties.VariableNames, {'E1', 'E2'});
%=============================================================================
T = cell2table(cell(0, 3));
asserts.isequal(size(T), [0 3]);
asserts.isequal(T.Properties.VariableNames, {'Var1', 'Var2', 'Var3'});
%=============================================================================
msg = _('The RowNames property must be a string array or a cell array, with each name containing one or more characters.');
asserts.checkerror('cell2table({1; 2}, ''RowNames'', ''a'')', msg, 'Nelson:table:invalidRowNames');
asserts.checkerror('cell2table({1; 2}, ''RowNames'', {''a'', ''''})', msg, 'Nelson:table:invalidRowNames');
asserts.checkerror('cell2table({1; 2}, ''RowNames'', [1 2])', msg, 'Nelson:table:invalidRowNames');
%=============================================================================
msg = _('To assign multiple variable names, specify nonempty names in a string array or a cell array of character vectors.');
asserts.checkerror('cell2table({1, 2}, ''VariableNames'', ''a'')', msg, 'Nelson:table:invalidVariableNames');
asserts.checkerror('cell2table({1; 2}, ''RowNames'', {''a''; ''b''}, ''VariableNames'', ''a'')', msg, 'Nelson:table:invalidVariableNames');
%=============================================================================
msg = _('Unknown parameter name.');
asserts.checkerror('cell2table({1, 2}, ''Foo'', 1)', msg, 'Nelson:table:unknownParameter');
asserts.checkerror('cell2table({1, 2}, ''Description'', ''x'')', msg, 'Nelson:table:unknownParameter');
%=============================================================================
msg = _('Name-value arguments must come in pairs.');
asserts.checkerror('cell2table({1, 2}, ''VariableNames'')', msg, 'Nelson:table:nameValuePair:nameValueArgumentsComePairs');
%=============================================================================
msg = _('Parameter name must be a character vector or string scalar.');
asserts.checkerror('cell2table({1, 2}, 3, 4)', msg, 'Nelson:table:invalidType:parameterNameACharacterVectorString');
%=============================================================================
msg = _('C must be a 2-D cell array.');
asserts.checkerror('cell2table([1 2])', msg, 'Nelson:table:invalidType:cA2DCellArray');
asserts.checkerror('cell2table(cell(2, 2, 2))', msg, 'Nelson:table:invalidType:cA2DCellArray');
%=============================================================================
