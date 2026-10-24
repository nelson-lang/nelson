%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a row repeated in the subscript of an assignment keeps its last value
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
R = T;
R{[1 2 1], 'A'} = [10; 20; 30];
assert_isequal(R.A, [30; 20]);
R = T;
R{[1 2 1], :} = [1 2; 3 4; 5 6];
assert_isequal(R{:, :}, [5 6; 3 4]);
R = T;
R([1 2 1], 'A') = table([10; 20; 30]);
assert_isequal(R.A, [30; 20]);
R = T;
R([1 2 1], :) = table([10; 20; 30], [1; 2; 3], 'VariableNames', {'A', 'B'});
assert_isequal(R{:, :}, [30 3; 20 2]);
R = T;
R([1 2 1], :) = {1, 2; 3, 4; 5, 6};
assert_isequal(R{:, :}, [5 6; 3 4]);
%=============================================================================
% a new variable
R = T;
R{[1 2 1], 'New'} = [1; 2; 3];
assert_isequal(R.New, [3; 2]);
warning('off', 'Nelson:table:rowsAddedNewVars');
R = T;
R{[1 1], 'C'} = [7; 8];
assert_isequal(R.C, [8; 0]);
R = T;
R([1 1], 'New') = table([1; 2], 'VariableNames', {'New'});
warning('on', 'Nelson:table:rowsAddedNewVars');
assert_isequal(R.New, [2; 0]);
%=============================================================================
% with rows added
warning('off', 'Nelson:table:rowsAddedExistingVars');
R = T;
R{[1 3 3], 'C'} = [7; 8; 9];
assert_isequal(R.C, [7; 0; 9]);
assert_isequal(R.A, [1; 2; 0]);
R = T;
R{[1 1 4], 'A'} = [5; 6; 7];
warning('on', 'Nelson:table:rowsAddedExistingVars');
assert_isequal(R.A, [6; 2; 0; 7]);
%=============================================================================
% a timetable
TT = timetable(seconds([1; 2]), [1; 2], 'VariableNames', {'A'});
R = TT;
R{[2 2], 'A'} = [5; 6];
assert_isequal(R.A, [1; 6]);
%=============================================================================
