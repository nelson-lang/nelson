%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Age = [38; 43; 38; 40; 49];
Height = [71; 69; 64; 67; 64];
T = table(Age, Height);
T.Properties.VariableUnits = {'yr', 'in'};
%=============================================================================
R = T * 2;
assert_isequal(R, T .* 2);
assert_isequal(R.Age, [76; 86; 76; 80; 98]);
assert_isequal(R.Properties.VariableUnits, {'yr', 'in'});
R = 0.25 * T;
assert_isequal(R, 0.25 .* T);
assert_isequal(R.Height, [17.75; 17.25; 16; 16.75; 16]);
%=============================================================================
R = T / 2;
assert_isequal(R, T ./ 2);
assert_isequal(R.Age, [19; 21.5; 19; 20; 24.5]);
R = 2 / T;
assert_isequal(R, 2 ./ T);
%=============================================================================
R = 2 \ T;
assert_isequal(R, 2 .\ T);
assert_isequal(R.Height, [35.5; 34.5; 32; 33.5; 32]);
R = T \ 2;
assert_isequal(R, T .\ 2);
%=============================================================================
msg = _('When one input is a table or timetable, the other input must be a scalar.');
assert_checkerror('T * T', msg);
assert_checkerror('T * T', msg, 'Nelson:table:mtimes:nonScalarOperand');
assert_checkerror('T * [1 2]', msg);
assert_checkerror('[1 2] / T', msg, 'Nelson:table:mrdivide:nonScalarOperand');
assert_checkerror('T \ T', msg, 'Nelson:table:mldivide:nonScalarOperand');
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [1; 2; 4], [10; 20; 40]);
R = TT * 3;
assert_istrue(istimetable(R));
assert_isequal(R, TT .* 3);
assert_isequal(R.Var2, [30; 60; 120]);
R = 4 / TT;
assert_isequal(R, 4 ./ TT);
assert_isequal(R.Var1, [4; 2; 1]);
R = TT / 2;
assert_isequal(R.Var1, [0.5; 1; 2]);
R = 2 \ TT;
assert_isequal(R, 2 .\ TT);
assert_isequal(R.Properties.RowTimes, TT.Properties.RowTimes);
assert_checkerror('TT * TT', msg, 'Nelson:table:mtimes:nonScalarOperand');
assert_checkerror('TT / ones(3, 2)', msg, 'Nelson:table:mrdivide:nonScalarOperand');
%=============================================================================
R = -T;
assert_isequal(R.Age, -Age);
assert_isequal(R.Properties.VariableUnits, {'yr', 'in'});
R = +T;
assert_isequal(R, T);
R = -TT;
assert_istrue(istimetable(R));
assert_isequal(R.Var2, [-10; -20; -40]);
R = +TT;
assert_isequal(R.Variables, TT.Variables);
assert_isequal(R.Properties.RowTimes, TT.Properties.RowTimes);
%=============================================================================
