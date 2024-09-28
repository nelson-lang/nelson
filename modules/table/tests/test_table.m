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
assert_istrue(isa(T, 'table'));
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
assert_istrue(istable(T));
assert_isequal(T{1,1}, {'Sanchez'});
assert_isequal(T{1,2}, 38);
assert_isequal(T{1,3}, true);
assert_isequal(T{1,4}, 71);
assert_isequal(T{1,5}, 176);
assert_isequal(T{1,6}, [124, 93]);
assert_isequal(T{2,1}, {'Johnson'}); 
assert_isequal(T{2,2}, 43);
assert_isequal(T{2,3}, false);
assert_isequal(T{2,4}, 69);
assert_isequal(T{2,5}, 163);
assert_isequal(T{2,6}, [109 77]);
%============================================================================
T = table([10;20;30],{'M';'F';'F'},'VariableNames',{'Age','Gender'},'RowNames',{'P1','P2','P3'});
assert_istrue(istable(T));
assert_isequal(T{1,1}, 10);
assert_isequal(T{1,2}, {'M'});
assert_isequal(T{2,1}, 20);
assert_isequal(T{2,2}, {'F'});
%============================================================================
LastName = {'Sanchez';'Johnson';'Lee';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
T = table(Age,Weight,Height,'RowNames',LastName);
assert_istrue(istable(T));
assert_isequal(T{1,1}, 38);
assert_isequal(T{1,2}, 176);
assert_isequal(T{1,3}, 71);
assert_isequal(T{2,1}, 43);
assert_isequal(T{2,2}, 163);
%============================================================================
FlightNum = [1261;547;3489];
Customer = ["Jones";"Brown";"Smith"];
Date = [1, 2, 3]';
Rating = ["Good";"Poor";"Fair"];
Comment = ["Flight left on time, not crowded";...
           "Late departure, ran out of dinner options";...
           "Late, but only by half an hour. Otherwise fine."];
T = table(FlightNum, Customer, Date, Rating, Comment);
assert_istrue(istable(T));
assert_isequal(T{1,1}, 1261);
assert_isequal(T{1,2}, "Jones");
assert_isequal(T{1,3}, 1);
assert_isequal(T{1,4}, "Good");
assert_isequal(T{1,5}, "Flight left on time, not crowded");
assert_isequal(T{2,1}, 547);
assert_isequal(T{2,2}, "Brown");
assert_isequal(T{2,3}, 2);
assert_isequal(T{2,4}, "Poor");
assert_isequal(T{2,5}, "Late departure, ran out of dinner options");
assert_isequal(T{3,1}, 3489);
assert_isequal(T{3,2}, "Smith");
assert_isequal(T{3,3}, 3);
assert_isequal(T{3,4}, "Fair");
assert_isequal(T{3,5}, "Late, but only by half an hour. Otherwise fine.");
%============================================================================
T = table(1, 2, 3);
assert_isequal(T.Var1, 1);
assert_isequal(T.Var2, 2);
assert_isequal(T.Var3, 3);
%============================================================================
