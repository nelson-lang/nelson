%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% RowNames and VariableNames are assigned through .Properties
%=============================================================================
id = 'Nelson:table:invalidPropertyAssignment';
msgRowNames = _('Assign to the ''RowNames'' property of a table using dot subscripting via .Properties.RowNames.');
msgVariableNames = _('Assign to the ''VariableNames'' property of a table using dot subscripting via .Properties.VariableNames.');
T = table([1; 2]);
assert_checkerror('T.RowNames = {''a''; ''b''};', msgRowNames, id);
assert_checkerror('T.VariableNames = {''x''};', msgVariableNames, id);
assert_isequal(T.Properties.VariableNames, {'Var1'});
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
assert_checkerror('TT.RowNames = {''a''; ''b''};', msgRowNames, id);
assert_checkerror('TT.VariableNames = {''x''};', msgVariableNames, id);
%=============================================================================
T.Properties.VariableNames = {'x'};
assert_isequal(T.x, [1; 2]);
T.y = [3; 4];
assert_isequal(T.Properties.VariableNames, {'x', 'y'});
%=============================================================================
