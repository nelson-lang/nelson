%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], [5; 6], 'VariableNames', {'A', 'B', 'C'});
T = movevars(T, 'C', 'Before', 1);
asserts.isequal(T.Properties.VariableNames, {'C', 'A', 'B'});
T = movevars(T, 'A', 'After', 'B');
asserts.isequal(T.Properties.VariableNames, {'C', 'B', 'A'});
%=============================================================================
% Moving a variable reorders the data and all per-variable properties, not
% only the names (previously table reconstruction failed).
T = table([1; 2; 3], {'a'; 'b'; 'c'}, [10; 20; 30], 'VariableNames', {'id', 'name', 'val'});
T2 = movevars(T, 'val', 'Before', 'id');
asserts.isequal(T2.Properties.VariableNames, {'val', 'id', 'name'});
asserts.isequal(T2.val, [10; 20; 30]);
asserts.isequal(T2.id, [1; 2; 3]);
T3 = movevars(T, 'id', 'After', 'val');
asserts.isequal(T3.Properties.VariableNames, {'name', 'val', 'id'});
asserts.isequal(T3.id, [1; 2; 3]);
%=============================================================================
