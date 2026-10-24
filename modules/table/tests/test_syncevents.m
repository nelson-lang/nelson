%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3; 4; 5]), [10; 20; 30; 40; 50], 'VariableNames', {'A'});
msg = _('Timetable must have an event table assigned to the Events property.');
assert_checkerror('syncevents(TT);', msg, 'Nelson:eventtable:NoAttachedEvents');
assert_checkerror('syncevents(TT, eventtable(seconds(2)));', msg, 'Nelson:eventtable:NoAttachedEvents');
%=============================================================================
% instantaneous events: the labels are copied to the rows at the event times
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', ["m"; "n"]);
R = syncevents(TT);
assert_isequal(class(R), 'timetable');
assert_isequal(R.Properties.VariableNames, {'A', 'EventLabels'});
assert_isequal(R.A, [10; 20; 30; 40; 50]);
assert_isequal(R.EventLabels([2, 4]), ["m"; "n"]);
assert_istrue(all(ismissing(R.EventLabels([1, 3, 5]))));
assert_istrue(isequal(R.Properties.Events, TT.Properties.Events));
%=============================================================================
% default label for the rows without event, converted to the label type
R = syncevents(TT, "none");
assert_isequal(R.EventLabels, ["none"; "m"; "none"; "n"; "none"]);
R = syncevents(TT, 'none');
assert_isequal(R.EventLabels, ["none"; "m"; "none"; "n"; "none"]);
R = syncevents(TT, 0);
assert_isequal(R.EventLabels, ["0"; "m"; "0"; "n"; "0"]);
%=============================================================================
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', categorical(["m"; "n"]));
R = syncevents(TT);
assert_isequal(class(R.EventLabels), 'categorical');
assert_isequal(isundefined(R.EventLabels), logical([1; 0; 1; 0; 1]));
R = syncevents(TT, "zz");
assert_isequal(string(R.EventLabels), ["zz"; "m"; "zz"; "n"; "zz"]);
%=============================================================================
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', [7; 8]);
R = syncevents(TT);
assert_isequal(R.EventLabels, [NaN; 7; NaN; 8; NaN]);
R = syncevents(TT, -1);
assert_isequal(R.EventLabels, [-1; 7; -1; 8; -1]);
%=============================================================================
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', {'m'; 'n'});
R = syncevents(TT);
assert_isequal(R.EventLabels, {''; 'm'; ''; 'n'; ''});
R = syncevents(TT, "d");
assert_isequal(R.EventLabels, {'d'; 'm'; 'd'; 'n'; 'd'});
%=============================================================================
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', [true; false]);
R = syncevents(TT);
assert_isequal(R.EventLabels, logical([0; 1; 0; 0; 0]));
%=============================================================================
% event times only: the generated labels
TT.Properties.Events = eventtable(seconds([2; 4]));
R = syncevents(TT);
assert_isequal(R.EventLabels([2, 4]), ["Event 1"; "Event 2"]);
%=============================================================================
% an event between two row times labels no row
TT.Properties.Events = eventtable(seconds(2.5), 'EventLabels', "m");
R = syncevents(TT);
assert_istrue(all(ismissing(R.EventLabels)));
%=============================================================================
% an empty event table adds nothing
TT.Properties.Events = eventtable(seconds(zeros(0, 1)));
R = syncevents(TT);
assert_isequal(R.Properties.VariableNames, {'A'});
assert_isequal(R.A, TT.A);
%=============================================================================
% datetime row times
TD = timetable(datetime(2020, 1, 1) + days(0:3)', [1; 2; 3; 4]);
TD.Properties.Events = eventtable(datetime(2020, 1, 2), 'EventLabels', "x");
R = syncevents(TD);
assert_isequal(R.EventLabels(2), "x");
assert_istrue(all(ismissing(R.EventLabels([1, 3, 4]))));
%=============================================================================
TT.Properties.Events = eventtable(seconds([2; 4]), 'EventLabels', ["m"; "n"]);
assert_checkerror('syncevents(TT, ["a", "b"]);', _('The default label supplied to syncevents must be a scalar.'), 'Nelson:eventtable:NonScalarLabel');
msg = sprintf(_('Invalid argument name ''%s''. Use ''EventDataVariables''.'), 'Foo');
assert_checkerror('syncevents(TT, ''Foo'', 1);', msg, 'Nelson:eventtable:invalidArgumentName');
msg = sprintf(_('Invalid argument name ''%s''. Use ''EventDataVariables''.'), 'a');
assert_checkerror('syncevents(TT, "a", "b");', msg, 'Nelson:eventtable:invalidArgumentName');
%=============================================================================
