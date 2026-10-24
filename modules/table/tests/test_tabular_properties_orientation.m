%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Per-variable metadata is stored as a row, whatever the orientation given.
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.VariableNames = {'a'; 'b'};
asserts.isequal(T.Properties.VariableNames, {'a', 'b'});
asserts.isequal(T.b, [3; 4]);
T.Properties.VariableUnits = {'m'; 's'};
asserts.isequal(T.Properties.VariableUnits, {'m', 's'});
T.Properties.VariableDescriptions = {'d1'; 'd2'};
asserts.isequal(T.Properties.VariableDescriptions, {'d1', 'd2'});
T.Properties.VariableContinuity = {'step'; 'event'};
asserts.isequal(size(T.Properties.VariableContinuity), [1 2]);
%=============================================================================
T = table([1; 2], [3; 4]);
T.Properties.VariableNames = ["p"; "q"];
asserts.isequal(T.Properties.VariableNames, {'p', 'q'});
T.Properties.VariableUnits = ["m"; "s"];
asserts.isequal(T.Properties.VariableUnits, {'m', 's'});
T.Properties.VariableDescriptions = ["a"; "b"];
asserts.isequal(T.Properties.VariableDescriptions, {'a', 'b'});
%=============================================================================
T = table([1; 2], [3; 4]);
T.x = [5; 6];
T.x = [];
asserts.isequal(T.Properties.VariableNames, {'Var1', 'Var2'});
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2], [3; 4]);
TT.Properties.VariableNames = {'a'; 'b'};
asserts.isequal(TT.Properties.VariableNames, {'a', 'b'});
asserts.isequal(TT.b, [3; 4]);
TT.Properties.VariableUnits = {'m'; 's'};
asserts.isequal(TT.Properties.VariableUnits, {'m', 's'});
TT.Properties.VariableDescriptions = ["d1"; "d2"];
asserts.isequal(TT.Properties.VariableDescriptions, {'d1', 'd2'});
R = TT(1, :);
asserts.isequal(R.Properties.VariableUnits, {'m', 's'});
TT = renamevars(TT, {'a'; 'b'}, {'c'; 'd'});
asserts.isequal(TT.Properties.VariableNames, {'c', 'd'});
%=============================================================================
