%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 2.05; 3; 4]), [1; 2; 3; 4; 5], ["a"; "b"; "c"; "d"; "e"], 'VariableNames', {'A', 'S'});
W = withtol(seconds(2), seconds(0.1));
assert_isequal(class(W), 'withtol');
assert_istrue(isa(W, 'withtol'));
%=============================================================================
% rows within the tolerance of each subscript time, subscript by subscript
R = TT(W, :);
assert_isequal(R.A, [2; 3]);
assert_isequal(R.Properties.RowTimes, seconds([2; 2.05]));
R = TT(withtol(seconds([3.9 2]), seconds(0.2)), :);
assert_isequal(R.A, [5; 2; 3]);
R = TT(withtol(seconds([3.9; 2]), seconds(0.2)), 'A');
assert_isequal(R.Properties.VariableNames, {'A'});
assert_isequal(R.A, [5; 2; 3]);
R = TT(withtol(seconds(10), seconds(0.1)), :);
assert_isequal(size(R), [0 2]);
R = TT(withtol(seconds(2.1), seconds(0.05)), :);
assert_isequal(R.A, 3);
R = TT(withtol(seconds(1), seconds(Inf)), :);
assert_isequal(R.A, [1; 2; 3; 4; 5]);
R = TT(withtol(seconds(zeros(0, 1)), seconds(1)), :);
assert_isequal(size(R), [0 2]);
%=============================================================================
assert_isequal(TT{W, 'A'}, [2; 3]);
assert_isequal(TT.A(W), [2; 3]);
%=============================================================================
R = TT;
R.A(withtol(seconds(2), seconds(0.1))) = 0;
assert_isequal(R.A, [1; 0; 0; 4; 5]);
R = TT;
R(W, :) = [];
assert_isequal(R.A, [1; 4; 5]);
%=============================================================================
TD = timetable(datetime(2020, 1, 1) + hours(0:3)', (1:4)');
R = TD(withtol(datetime(2020, 1, 1, 1, 10, 0), minutes(15)), :);
assert_isequal(R.Var1, 2);
R = TD(withtol('2020-01-01', minutes(15)), :);
assert_isequal(R.Var1, 1);
%=============================================================================
msg = sprintf(_('A timetable with %s row times cannot be subscripted using %s values.'), 'datetime', 'duration');
assert_checkerror('TD(withtol(seconds(1), seconds(1)), :);', msg, 'Nelson:withtol:MismatchRowTimesType');
msg = sprintf(_('A timetable with %s row times cannot be subscripted using %s values.'), 'duration', 'datetime');
assert_checkerror('TT(withtol(datetime(2020, 1, 1), seconds(1)), :);', msg, 'Nelson:withtol:MismatchRowTimesType');
msg = _('Tolerance value must be a nonnegative scalar duration.');
assert_checkerror('withtol(seconds(1), seconds(-1));', msg, 'Nelson:withtol:InvalidTolerance');
assert_checkerror('withtol(seconds(1), seconds([1 2]));', msg, 'Nelson:withtol:InvalidTolerance');
assert_checkerror('withtol(seconds(1), 1);', msg, 'Nelson:withtol:InvalidTolerance');
assert_checkerror('withtol(datetime(2020, 1, 1), caldays(1));', msg, 'Nelson:withtol:InvalidTolerance');
assert_checkerror('withtol(1, seconds(1));', _('Specify times for subscripting as datetime or duration.'), 'Nelson:timetable:InvalidTimes');
assert_checkerror('withtol(seconds(1));', _('Not enough input arguments.'), 'Nelson:narginchk:notEnoughInputs');
%=============================================================================
% a tolerance of half the smallest interval between subscript times could
% select a row twice; repeated subscript times are allowed
msg = sprintf(_('Tolerance exceeds half the smallest interval between subscript times and might result in selecting duplicate rows. Tolerance must be less than %s.'), '0.025 sec');
assert_checkerror('withtol(seconds([2 2.05]), seconds(0.1));', msg, 'Nelson:withtol:LargeTolerance');
msg = sprintf(_('Tolerance exceeds half the smallest interval between subscript times and might result in selecting duplicate rows. Tolerance must be less than %s.'), '0.5 sec');
assert_checkerror('withtol(seconds([2 3]), seconds(0.5));', msg, 'Nelson:withtol:LargeTolerance');
W = withtol(seconds([2 2]), seconds(0.1));
assert_isequal(TT(W, :).A, [2; 3; 2; 3]);
%=============================================================================
% display
txt = evalc('disp(withtol(seconds([2 3.9]), seconds(0.2)))');
assert_istrue(contains(txt, 'timetable withtol subscript:'));
assert_istrue(contains(txt, '3.9 sec'));
assert_istrue(contains(txt, 'with tolerance of +/- 0.2 sec'));
txt = evalc('disp(withtol())');
assert_istrue(contains(txt, 'NaT'));
txt = evalc('S = withtol(seconds(2), seconds(0.1))');
assert_istrue(startsWith(txt, sprintf('\nS = \n')));
assert_istrue(contains(txt, 'with tolerance of +/- 0.1 sec'));
%=============================================================================
