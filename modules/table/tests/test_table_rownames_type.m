%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% RowNames requires a string array or a cell array of nonempty names.
%=============================================================================
msg = _('The RowNames property must be a string array or a cell array, with each name containing one or more characters.');
id = 'Nelson:table:invalidRowNames';
asserts.checkerror('table([1; 2], ''RowNames'', ''a'')', msg, id);
asserts.checkerror('table(1, ''RowNames'', ''a'')', msg, id);
asserts.checkerror('table([1; 2], ''RowNames'', 5)', msg, id);
asserts.checkerror('table([1; 2], ''RowNames'', {''a''; ''''})', msg, id);
asserts.checkerror('table(''Size'', [2 1], ''VariableTypes'', {''double''}, ''RowNames'', ''a'')', msg, id);
asserts.checkerror('array2table([1; 2], ''RowNames'', ''a'')', msg, id);
%=============================================================================
T = table([1; 2]);
asserts.checkerror('T.Properties.RowNames = ''a'';', msg, id);
asserts.isequal(T.Properties.RowNames, {});
T1 = table(1);
asserts.checkerror('T1.Properties.RowNames = ''a'';', msg, id);
p = T1.Properties;
p.RowNames = 'a';
asserts.checkerror('T1.Properties = p;', msg, id);
p = rmfield(T1.Properties, 'VariableUnits');
p.RowNames = 'a';
asserts.checkerror('T1.Properties = p;', msg, id);
asserts.isequal(T1.Properties.RowNames, {});
%=============================================================================
T = table([1; 2], 'RowNames', ["a"; "b"]);
asserts.isequal(T.Properties.RowNames, {'a'; 'b'});
T = table([1; 2], 'RowNames', {'a', 'b'});
asserts.isequal(T.Properties.RowNames, {'a'; 'b'});
T.Properties.RowNames = ["c", "d"];
asserts.isequal(T.Properties.RowNames, {'c'; 'd'});
T.Properties.RowNames = {};
asserts.isequal(T.Properties.RowNames, {});
T = array2table([1; 2], 'RowNames', {'a'; 'b'});
asserts.isequal(T.Properties.RowNames, {'a'; 'b'});
R = T(2, :);
asserts.isequal(R.Properties.RowNames, {'b'});
%=============================================================================
