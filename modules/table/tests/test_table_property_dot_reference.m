%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% reading a property by dot name points to .Properties
%=============================================================================
T = table([1; 2]);
msg = sprintf(_('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), 'RowNames', 'RowNames');
assert_checkerror('x = T.RowNames;', msg, 'Nelson:table:illegalPropertyReference');
msg = sprintf(_('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), 'VariableNames', 'VariableNames');
assert_checkerror('x = T.VariableNames;', msg, 'Nelson:table:illegalPropertyReference');
msg = sprintf(_('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), 'Description', 'Description');
assert_checkerror('x = T.Description;', msg, 'Nelson:table:illegalPropertyReference');
assert_checkerror('x = T.Foo;', sprintf(_('Unrecognized table variable name ''%s''.'), 'Foo'), 'Nelson:table:unrecognizedVariable');
%=============================================================================
% a variable of that name is read
T = table([1; 2], 'VariableNames', {'Description'});
assert_isequal(T.Description, [1; 2]);
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
msg = sprintf(_('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), 'VariableNames', 'VariableNames');
assert_checkerror('x = TT.VariableNames;', msg, 'Nelson:table:illegalPropertyReference');
msg = sprintf(_('Access the ''%s'' property of a table using dot subscripting via .Properties.%s.'), 'RowTimes', 'RowTimes');
assert_checkerror('x = TT.RowTimes;', msg, 'Nelson:table:illegalPropertyReference');
assert_checkerror('x = TT.RowNames;', sprintf(_('Unrecognized table variable name ''%s''.'), 'RowNames'), 'Nelson:timetable:unrecognizedVariable');
%=============================================================================
