%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table();
assert_istrue(isempty(T));
%=============================================================================
Smoker = ["Y";"Y";"N";"N";"N"];
Age = [38;43;38;40;49];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
VariableNames = ["Smoker" "Age" "BloodPressure"];
RowNames = ["Chang" "Brown" "Ruiz" "Lee" "Garcia"];
T = table(Smoker, Age, BloodPressure, 'VariableNames',VariableNames, 'RowNames', RowNames);
assert_isfalse(isempty(T));
%=============================================================================
