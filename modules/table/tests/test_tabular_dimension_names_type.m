%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% DimensionNames requires two distinct nonempty names, given as a string
% array or a cell array, that are not variable names.
%=============================================================================
msgType = _('The DimensionNames property must be a string array or a cell array, with each name containing one or more characters.');
idType = 'Nelson:table:invalidDimensionNames';
msgCount = _('The DimensionNames property must contain one name for each dimension in the table.');
idCount = 'Nelson:table:invalidDimensionNamesCount';
asserts.checkerror('table([1; 2], ''DimensionNames'', ''x'')', msgType, idType);
asserts.checkerror('table([1; 2], ''DimensionNames'', 5)', msgType, idType);
asserts.checkerror('table([1; 2], ''DimensionNames'', {''x'', ''''})', msgType, idType);
asserts.checkerror('table([1; 2], ''DimensionNames'', {''x''})', msgCount, idCount);
asserts.checkerror('table([1; 2], ''DimensionNames'', {''x'', ''y'', ''z''})', msgCount, idCount);
asserts.checkerror('table([1; 2], ''DimensionNames'', {''x'', ''x''})', sprintf(_('Duplicate table dimension name: ''%s''.'), 'x'), 'Nelson:table:duplicateDimensionNames');
asserts.checkerror('table([1; 2], ''DimensionNames'', {''Var1'', ''y''})', sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Var1'), 'Nelson:table:duplicateDimensionVariableNames');
asserts.checkerror('table(''Size'', [2 1], ''VariableTypes'', {''double''}, ''DimensionNames'', ''x'')', msgType, idType);
asserts.checkerror('array2table([1; 2], ''DimensionNames'', ''x'')', msgType, idType);
asserts.checkerror('cell2table({1; 2}, ''DimensionNames'', ''x'')', msgType, idType);
asserts.checkerror('timetable(seconds([1; 2]), [1; 2], ''DimensionNames'', ''x'')', msgType, idType);
%=============================================================================
T = table([1; 2]);
asserts.checkerror('T.Properties.DimensionNames = ''x'';', msgType, idType);
asserts.checkerror('T.Properties.DimensionNames = {''x''};', msgCount, idCount);
asserts.checkerror('T.Properties.DimensionNames = {''Var1'', ''y''};', sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Var1'), 'Nelson:table:duplicateDimensionVariableNames');
asserts.checkerror('T.Properties.VariableNames = {''Row''};', sprintf(_('Duplicate dimension and variable name: ''%s''.'), 'Row'), 'Nelson:table:duplicateDimensionVariableNames');
p = rmfield(T.Properties, 'VariableUnits');
p.DimensionNames = 'x';
asserts.checkerror('T.Properties = p;', msgType, idType);
asserts.isequal(T.Properties.DimensionNames, {'Row', 'Variables'});
TT = timetable(seconds([1; 2]), [1; 2]);
asserts.checkerror('TT.Properties.DimensionNames = ''x'';', msgType, idType);
asserts.isequal(TT.Properties.DimensionNames, {'Time', 'Variables'});
%=============================================================================
T = table([1; 2], 'DimensionNames', ["x", "y"]);
asserts.isequal(T.Properties.DimensionNames, {'x', 'y'});
T = table([1; 2], 'DimensionNames', {'x'; 'y'});
asserts.isequal(T.Properties.DimensionNames, {'x', 'y'});
T.Properties.DimensionNames = ["a", "b"];
asserts.isequal(T.Properties.DimensionNames, {'a', 'b'});
T = array2table([1; 2], 'DimensionNames', {'r', 'v'});
asserts.isequal(T.Properties.DimensionNames, {'r', 'v'});
T = cell2table({1; 2}, 'DimensionNames', {'r', 'v'});
asserts.isequal(T.Properties.DimensionNames, {'r', 'v'});
%=============================================================================
