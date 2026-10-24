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
asserts.istrue(isa(T, 'table'));
%=============================================================================
LastName = {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'};
Age = [38; 43; 38; 40; 49];
Smoker = logical([1; 0; 1; 0; 1]);
Height = [71; 69; 64; 67; 64];
Weight = [176; 163; 131; 133; 119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
asserts.istrue(istable(T));
asserts.isequal(T{1, 1}, {'Sanchez'});
asserts.isequal(T{1, 2}, 38);
asserts.isequal(T{1, 3}, true);
asserts.isequal(T{1, 4}, 71);
asserts.isequal(T{1, 5}, 176);
asserts.isequal(T{1, 6}, [124, 93]);
asserts.isequal(T{2, 1}, {'Johnson'});
asserts.isequal(T{2, 2}, 43);
asserts.isequal(T{2, 3}, false);
asserts.isequal(T{2, 4}, 69);
asserts.isequal(T{2, 5}, 163);
asserts.isequal(T{2, 6}, [109 77]);
%============================================================================
T = table([10; 20; 30], {'M'; 'F'; 'F'}, 'VariableNames', {'Age', 'Gender'}, 'RowNames', {'P1', 'P2', 'P3'});
asserts.istrue(istable(T));
asserts.isequal(T{1, 1}, 10);
asserts.isequal(T{1, 2}, {'M'});
asserts.isequal(T{2, 1}, 20);
asserts.isequal(T{2, 2}, {'F'});
%============================================================================
LastName = {'Sanchez'; 'Johnson'; 'Lee'; 'Diaz'; 'Brown'};
Age = [38; 43; 38; 40; 49];
Height = [71; 69; 64; 67; 64];
Weight = [176; 163; 131; 133; 119];
T = table(Age, Weight, Height, 'RowNames', LastName);
asserts.istrue(istable(T));
asserts.isequal(T{1, 1}, 38);
asserts.isequal(T{1, 2}, 176);
asserts.isequal(T{1, 3}, 71);
asserts.isequal(T{2, 1}, 43);
asserts.isequal(T{2, 2}, 163);
%============================================================================
FlightNum = [1261; 547; 3489];
Customer = ["Jones"; "Brown"; "Smith"];
Date = [1, 2, 3]';
Rating = ["Good"; "Poor"; "Fair"];
Comment = ["Flight left on time, not crowded"; ...
           "Late departure, ran out of dinner options"; ...
           "Late, but only by half an hour. Otherwise fine."];
T = table(FlightNum, Customer, Date, Rating, Comment);
asserts.istrue(istable(T));
asserts.isequal(T{1, 1}, 1261);
asserts.isequal(T{1, 2}, "Jones");
asserts.isequal(T{1, 3}, 1);
asserts.isequal(T{1, 4}, "Good");
asserts.isequal(T{1, 5}, "Flight left on time, not crowded");
asserts.isequal(T{2, 1}, 547);
asserts.isequal(T{2, 2}, "Brown");
asserts.isequal(T{2, 3}, 2);
asserts.isequal(T{2, 4}, "Poor");
asserts.isequal(T{2, 5}, "Late departure, ran out of dinner options");
asserts.isequal(T{3, 1}, 3489);
asserts.isequal(T{3, 2}, "Smith");
asserts.isequal(T{3, 3}, 3);
asserts.isequal(T{3, 4}, "Fair");
asserts.isequal(T{3, 5}, "Late, but only by half an hour. Otherwise fine.");
%============================================================================
T = table(1, 2, 3);
asserts.isequal(T.Var1, 1);
asserts.isequal(T.Var2, 2);
asserts.isequal(T.Var3, 3);
%============================================================================
LastName = {'Sanchez', 'Johnson', 'Li', 'Diaz', 'Brown'};
Age = [38; 43; 38; 40; 49];
Height = [71; 69; 64; 67; 64];
Weight = [176; 163; 131; 133; 119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
msg = sprintf(_('%s must have the same number of rows.'), 'All table variables');
asserts.checkerror('T1 = table(LastName, Age, Height, Weight, BloodPressure)', msg);
%============================================================================
Name = {'Ann'; 'Bob'; 'Cid'};
Age = [10; 20; 30];
Height = [100; 120; 140];
T = table(Name, Age, Height, 'RowNames', {'r1', 'r2', 'r3'});
asserts.istrue(istable(T));
asserts.isequal(size(T), [3 3]);
asserts.isequal(height(T), 3);
asserts.isequal(width(T), 3);
asserts.isequal(T.Properties.VariableNames, {'Name', 'Age', 'Height'});
asserts.isequal(T.Age, Age);
asserts.isequal(T{'r2', {'Age', 'Height'}}, [20 120]);
%=============================================================================
HeadRows = head(T, 2);
TailRows = tail(T, 1);
asserts.isequal(height(HeadRows), 2);
asserts.isequal(table2cell(TailRows(:, {'Name', 'Age', 'Height'})), {'Cid', 30, 140});
TAdded = addvars(table(Age, Height), Name, 'NewVariableNames', {'Name'}, 'Before', 'Age');
asserts.isequal(TAdded.Properties.VariableNames, {'Name', 'Age', 'Height'});
TRenamed = renamevars(TAdded, 'Age', 'Years');
asserts.isequal(TRenamed.Properties.VariableNames, {'Name', 'Years', 'Height'});
TNoHeight = removevars(TRenamed, 'Height');
asserts.isequal(TNoHeight.Properties.VariableNames, {'Name', 'Years'});
TMoved = movevars(TRenamed, 'Height', 'Before', 'Years');
asserts.isequal(TMoved.Properties.VariableNames, {'Name', 'Height', 'Years'});
%=============================================================================
T.Properties.VariableDescriptions = {'label', 'years', 'cm'};
T.Properties.VariableUnits = {'', 'yr', 'cm'};
Summary = summary(T);
asserts.isequal(Summary.Age.Size, [3 1]);
asserts.isequal(Summary.Age.Units, 'yr');
asserts.isequal(Summary.Age.Min, 10);
asserts.isequal(Summary.Age.Median, 20);
asserts.isequal(Summary.Height.Max, 140);
%=============================================================================
FromArray = array2table([1 4; 2 5]);
asserts.isequal(FromArray.Var1, [1; 2]);
asserts.isequal(FromArray.Var2, [4; 5]);
FromCell = cell2table({1, 'a'; 2, 'b'}, 'VariableNames', {'A', 'B'});
asserts.isequal(table2cell(FromCell), {1, 'a'; 2, 'b'});
FromStruct = struct2table(struct('A', {1; 2}, 'B', {'a'; 'b'}));
asserts.isequal(FromStruct.B, {'a'; 'b'});
%=============================================================================
RTable = table([1; 2], [3; 4], [5 6; 7 8], 'VariableNames', {'A', 'B', 'Pair'});
Merged = mergevars(RTable, {'A', 'B'}, 'NewVariableName', 'AB');
asserts.isequal(Merged.Properties.VariableNames, {'AB', 'Pair'});
asserts.isequal(Merged.AB, [1 3; 2 4]);
Split = splitvars(RTable, 'Pair', 'NewVariableNames', {'P1', 'P2'});
asserts.isequal(Split.Properties.VariableNames, {'A', 'B', 'P1', 'P2'});
Rows = rows2vars(table({'r1'; 'r2'}, [10; 20], 'VariableNames', {'Key', 'Value'}), ...
  'VariableNamesSource', 'Key');
asserts.isequal(Rows.Properties.VariableNames, {'OriginalVariableNames', 'r1', 'r2'});
%=============================================================================
MissingTable = table([1; NaN; 3], {'a'; ''; 'c'}, 'VariableNames', {'A', 'B'});
asserts.isequal(ismissing(MissingTable), [false false; true true; false false]);
Filled = fillmissing(MissingTable, 'constant', 0, 'DataVariables', 'A');
asserts.isequal(Filled.A, [1; 0; 3]);
asserts.isequal(rmmissing(MissingTable).A, [1; 3]);
Standardized = standardizeMissing(table([1; -99; 3], 'VariableNames', {'A'}), -99);
asserts.istrue(isnan(Standardized.A(2)));
%=============================================================================
GroupTable = table({'a'; 'a'; 'b'}, [1; 2; 4], [10; 20; 30], ...
  'VariableNames', {'G', 'X', 'Y'});
VarResult = varfun(@mean, GroupTable, 'InputVariables', vartype('double'));
asserts.isequal(table2array(VarResult), [7 / 3 20]);
RowResult = rowfun(@(x, y) x + y, GroupTable, 'InputVariables', {'X', 'Y'}, ...
  'OutputVariableNames', 'Sum');
asserts.isequal(RowResult.Sum, [11; 22; 34]);
Counts = groupcounts(GroupTable, 'G');
asserts.isequal(Counts.GroupCount, [2; 1]);
Grouped = groupsummary(GroupTable, 'G', 'sum', {'X', 'Y'});
asserts.isequal(Grouped.sum_X, [3; 4]);
asserts.isequal(Grouped.sum_Y, [30; 30]);
%=============================================================================
Left = table([1; 2; 3], {'A'; 'B'; 'C'}, 'VariableNames', {'Key', 'LeftValue'});
Right = table([2; 3; 4], [20; 30; 40], 'VariableNames', {'Key', 'RightValue'});
Inner = innerjoin(Left, Right, 'Keys', 'Key');
asserts.isequal(Inner.Key, [2; 3]);
asserts.isequal(Inner.RightValue, [20; 30]);
joinFailed = false;
try
  join(Left, Right, 'Keys', 'Key');
catch
  joinFailed = true;
end
asserts.istrue(joinFailed);
Outer = outerjoin(Left, Right, 'Keys', 'Key', 'MergeKeys', true);
asserts.isequal(Outer.Key, [1; 2; 3; 4]);
%=============================================================================
StackInput = table({'A'; 'B'}, [1; 2], [3; 4], 'VariableNames', {'ID', 'X', 'Y'});
Stacked = stack(StackInput, {'X', 'Y'}, 'NewDataVariableName', 'Value', ...
  'IndexVariableName', 'Measure');
asserts.isequal(height(Stacked), 4);
asserts.isequal(Stacked.Value, [1; 3; 2; 4]);
Unstacked = unstack(Stacked, 'Value', 'Measure', 'GroupingVariables', 'ID');
asserts.isequal(Unstacked.X, [1; 2]);
asserts.isequal(Unstacked.Y, [3; 4]);
asserts.checkerror('table2array(1)', sprintf(_('%s must be a table.'), 'T'));
asserts.checkerror('T.missing', sprintf(_('Unrecognized table variable name ''%s''.'), 'missing'));
asserts.checkerror('T{''missing'', ''Age''}', sprintf(_('Unrecognized table row name ''%s''.'), 'missing'));
asserts.checkerror('renamevars(T, ''Age'', ''Height'')', sprintf(_('%s names must be nonempty and distinct.'), 'Variable'));
asserts.checkerror('removevars(T, ''missing'')', sprintf(_('Unrecognized table variable name ''%s''.'), 'missing'));
asserts.checkerror('addvars(T, [9; 8], ''NewVariableNames'', {''Score''})', ...
  sprintf(_('%s must have the same number of rows.'), 'All table variables'));
%=============================================================================
PropTable = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
PropTable = addprop(PropTable, 'Source', 'table');
PropTable.Properties.CustomProperties.Source = 'compatibility';
asserts.isequal(PropTable.Properties.CustomProperties.Source, 'compatibility');
PropTable = addprop(PropTable, {'LowerLimit', 'UpperLimit'}, 'variable');
PropTable.Properties.CustomProperties.LowerLimit = [0 0];
PropTable.Properties.CustomProperties.UpperLimit = [10 20];
asserts.isequal(PropTable.Properties.CustomProperties.LowerLimit, [0 0]);
asserts.isequal(PropTable.Properties.CustomProperties.UpperLimit, [10 20]);
PropTable = rmprop(PropTable, 'LowerLimit');
asserts.isfalse(isfield(PropTable.Properties.CustomProperties, 'LowerLimit'));
asserts.istrue(isfield(PropTable.Properties.CustomProperties, 'UpperLimit'));
PropTable = rmprop(PropTable, {'Source', 'UpperLimit'});
asserts.isfalse(isfield(PropTable.Properties.CustomProperties, 'Source'));
asserts.isfalse(isfield(PropTable.Properties.CustomProperties, 'UpperLimit'));
%=============================================================================
