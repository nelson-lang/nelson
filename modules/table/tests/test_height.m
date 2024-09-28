%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
assert_isequal(height(T), 5);
%=============================================================================
assert_isequal(height(ones(4,3)), 4);
%=============================================================================
T = table(ones(2,1,3),2*ones(2,2,3),3*ones(2,3,3),'VariableNames',["One" "Two" "Three"]);
assert_isequal(height(T), 2);
%=============================================================================
