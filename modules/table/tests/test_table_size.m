%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table();
assert_isequal(size(T), [0 0]);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
sz = size(T);
assert_isequal(sz, [5 6]);
%=============================================================================
T = table([10;20;30],{'M';'F';'F'},'VariableNames',{'Age','Gender'},'RowNames',{'P1','P2','P3'});
sz = size(T);
assert_isequal(sz, [3 2]);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Lee';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
T = table(Age, Weight, Height, 'RowNames', LastName);
sz = size(T);
assert_isequal(sz, [5 3]);
%=============================================================================
FlightNum = [1261;547;3489];
Customer = ["Jones";"Brown";"Smith"];
Date = [1, 2, 3]';
Rating = ["Good";"Poor";"Fair"];
Comment = ["Flight left on time, not crowded";
           "Late departure, ran out of dinner options";
           "Late, but only by half an hour. Otherwise fine."];
T = table(FlightNum, Customer, Date, Rating, Comment);
sz = size(T);
assert_isequal(sz, [3 5]);
%=============================================================================
R = size(T, 1);
assert_isequal(R, 3);
%=============================================================================
R = size(T, 2);
assert_isequal(R, 5);
%=============================================================================
R = size(T, 3);
assert_isequal(R, 1);
%=============================================================================
[A, B, C, D] = size(T);
assert_isequal(A, 3);
assert_isequal(B, 5);
assert_isequal(C, 1);
assert_isequal(D, 1);
%=============================================================================
Smoker = ["Y";"Y";"N";"N";"N"];
Age = [38;43;38;40;49];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
VariableNames = ["Smoker" "Age" "BloodPressure"];
RowNames = ["Chang" "Brown" "Ruiz" "Lee" "Garcia"];
T = table(Smoker, Age, BloodPressure, 'VariableNames',VariableNames, 'RowNames', RowNames);
assert_isequal(size(T), [5, 3]);
%=============================================================================
T = table(ones(2,1,3),2*ones(2,2,3),3*ones(2,3,3),'VariableNames',["One" "Two" "Three"]);
assert_isequal(size(T), [2, 3]);
%=============================================================================
