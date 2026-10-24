%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 'VariableNames' requires a string array or a cell array of character vectors.
%=============================================================================
msg = _('To assign multiple variable names, specify nonempty names in a string array or a cell array of character vectors.');
id = 'Nelson:table:invalidVariableNames';
asserts.checkerror('table([1; 2], ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('table([1; 2], [3; 4], ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('table([1; 2], ''VariableNames'', 5)', msg, id);
asserts.checkerror('table(''Size'', [2 1], ''VariableTypes'', {''double''}, ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('timetable(seconds([1; 2]), [1; 2], ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('timetable(''Size'', [2 1], ''VariableTypes'', {''double''}, ''RowTimes'', seconds([1; 2]), ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('array2table([1; 2], ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('array2timetable([1; 2], ''RowTimes'', seconds([1; 2]), ''VariableNames'', ''z'')', msg, id);
asserts.checkerror('cell2table({1; 2}, ''VariableNames'', ''z'')', msg, id);
%=============================================================================
T = table([1; 2], 'VariableNames', "z");
asserts.isequal(T.Properties.VariableNames, {'z'});
T = table([1; 2], 'VariableNames', {'z'});
asserts.isequal(T.Properties.VariableNames, {'z'});
T = table([1; 2], [3; 4], 'VariableNames', ["a", "b"]);
asserts.isequal(T.Properties.VariableNames, {'a', 'b'});
T = table('Size', [2 1], 'VariableTypes', {'double'}, 'VariableNames', {'z'});
asserts.isequal(T.Properties.VariableNames, {'z'});
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', "z");
asserts.isequal(TT.Properties.VariableNames, {'z'});
TT = array2timetable([1; 2], 'RowTimes', seconds([1; 2]), 'VariableNames', {'z'});
asserts.isequal(TT.Properties.VariableNames, {'z'});
T = cell2table({1; 2}, 'VariableNames', {'z'});
asserts.isequal(T.Properties.VariableNames, {'z'});
%=============================================================================
