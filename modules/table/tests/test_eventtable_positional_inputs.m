%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% event data come only from a timetable first input
TT = timetable(seconds([2; 4]), [10; 20], 'VariableNames', {'X'});
msg = 'Invalid argument at position 2. Function requires 0 to 1 positional input(s).';
assert_checkerror('eventtable(seconds([2; 4]), [1; 2])', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], [3; 4])', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(datetime(2020, 1, [1; 2]), [1; 2])', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(seconds([2; 4]), {1; 2})', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(TT, [1; 2])', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(TT, TT)', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(1, 2)', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(seconds([2; 4]), "foo")', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], ''EventLabels'')', msg, 'Nelson:eventtable:tooManyInputs');
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], ''EventLabels'', "m", 5, 6)', msg, 'Nelson:eventtable:tooManyInputs');
%=============================================================================
msg = 'Invalid argument at position 2. Function requires 0 to 1 positional input(s) before name ''EventLabels''.';
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], ''EventLabels'', ["m"; "n"])', msg, 'Nelson:eventtable:tooManyInputs:beforeName');
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], "EventLabels", "q")', msg, 'Nelson:eventtable:tooManyInputs:beforeName');
assert_checkerror('eventtable(TT, [1; 2], [3; 4], ''EventLabels'', ["m"; "n"])', msg, 'Nelson:eventtable:tooManyInputs:beforeName');
msg = 'Invalid argument at position 2. Function requires 0 to 1 positional input(s) before name ''EventLengths''.';
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], ''EventLengths'', seconds(1))', msg, 'Nelson:eventtable:tooManyInputs:beforeName');
%=============================================================================
% a name-value name is checked before the positional count
msg = 'Invalid argument name ''Foo''. Name must be ''EventLabels'', ''EventLabelsVariable'', ''EventLengths'', ''EventLengthsVariable'', ''EventEnds'', or ''EventEndsVariable''.';
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], ''Foo'', 1)', msg, 'Nelson:eventtable:invalidArgumentName');
assert_checkerror('eventtable(seconds([2; 4]), [1; 2], ''EventLabels'', 1, ''Foo'', "q")', msg, 'Nelson:eventtable:invalidArgumentName');
msg = 'Invalid argument name ''q''. Name must be ''EventLabels'', ''EventLabelsVariable'', ''EventLengths'', ''EventLengthsVariable'', ''EventEnds'', or ''EventEndsVariable''.';
assert_checkerror('eventtable(seconds([2; 4]), ''EventLabels'', "q", "Foo")', msg, 'Nelson:eventtable:invalidArgumentName');
%=============================================================================
% a name without its value
msg = 'Invalid argument list. Make sure name-value arguments include both the name and the value.';
assert_checkerror('eventtable(seconds([2; 4]), ''EventLabels'')', msg, 'Nelson:eventtable:nameValuePair');
assert_checkerror('eventtable(seconds([2; 4]), ''EventEnds'')', msg, 'Nelson:eventtable:nameValuePair');
assert_checkerror('eventtable(seconds([2; 4]), "EventLabels", ["m"; "n"], [1; 2])', msg, 'Nelson:eventtable:nameValuePair');
assert_checkerror('eventtable(seconds([2; 4]), ''EventLabels'', "m", 5, 6)', msg, 'Nelson:eventtable:nameValuePair');
%=============================================================================
% the first input is checked after the name-value arguments
msg = 'First input must either be a timetable or it must be a datetime or duration vector.';
assert_checkerror('eventtable(1)', msg, 'Nelson:eventtable:invalidEventTimes');
assert_checkerror('eventtable("abc")', msg, 'Nelson:eventtable:invalidEventTimes');
assert_checkerror('eventtable(''EventLabels'', "m")', msg, 'Nelson:eventtable:invalidEventTimes');
assert_checkerror('eventtable(1, ''EventLabels'', "q")', msg, 'Nelson:eventtable:invalidEventTimes');
%=============================================================================
E = eventtable(seconds([2; 4]), 'EventLabels', ["m"; "n"]);
assert_isequal(size(E), [2 1]);
assert_isequal(E.Properties.VariableNames, {'EventLabels'});
assert_isequal(E.EventLabels, ["m"; "n"]);
E = eventtable(seconds([2; 4]), 'eventlabels', "m", 'EventLabels', "q");
assert_isequal(E.EventLabels, ["q"; "q"]);
E = eventtable(TT, 'EventLabelsVariable', 'X');
assert_isequal(size(E), [2 1]);
assert_isequal(E.Properties.VariableNames, {'X'});
assert_isequal(E.Properties.EventLabelsVariable, 'X');
%=============================================================================
