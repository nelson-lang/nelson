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
filename = [tempdir(), 'test_writetable_1.csv'];
writetable(T, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_1.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45 45;41 32;40 34],{'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_2.csv'];
writetable(T, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_2.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
filename = [tempdir(), 'test_writetable_3.csv'];
writetable(T, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_3.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['MF';'FM';'MF'],[45 45;41 32;40 34],{'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_4.csv'];
writetable(T, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_4.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45 45;41 32;40 34], {'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_5.csv'];
writetable(T,filename,'Delimiter',' ')  
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_5.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
filename = [tempdir(), 'test_writetable_6.csv'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', true)  
R = fileread(filename);    
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_6.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45;41;36],  {'New York, NY';'San Diego, CA';'Boston, MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_7.csv'];
writetable(T, filename, 'Delimiter', ',', 'QuoteStrings','all');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_7.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45;41;36],  {'New York'', NY';'San "Diego, CA';'Boston, MA'},[true;false;false], 'VariableNames', {'A'' B', 'B"" D', 'C" E', 'D F'});
filename = [tempdir(), 'test_writetable_8.csv'];
writetable(T, filename, 'Delimiter', ',', 'QuoteStrings','minimal');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_8.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45;41;36],  {'New York'', NY';'San "Diego, CA';'Boston, MA'},[true;false;false], 'VariableNames', {'A'' B', 'B ""D', 'C" E', 'D F'});
filename = [tempdir(), 'test_writetable_9.csv'];
writetable(T, filename, 'Delimiter', ' ', 'QuoteStrings','minimal');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_9.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45;41;36],  {'New York'', NY';'San "Diego, CA';'Boston, MA'},[true;false;false], 'VariableNames', {'A'' B', 'B ""D', 'C" E', 'D F'});
filename = [tempdir(), 'test_writetable_10.csv'];
writetable(T, filename, 'Delimiter', ' ', 'QuoteStrings','minimal', 'WriteVariableNames', false);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_10.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45 45;41 32;40 34],{'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_11.csv'];
writetable(T, filename);
writetable(T, filename, 'WriteMode', 'append', 'WriteVariableNames', false);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_11.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = logical([0 1; 1 0; 1 1; 0 0; 1 0]);
filename = [tempdir(), 'test_writetable_12.csv'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', false)  
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_12.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = ["Smith"; NaN;"Williams";"Jones";"Brown"];
Age = int32([38;43;38;40;49]);
Height = sparse([71;69;64;67;64]);
Weight = int8([176;163;131;133;119]);
BloodPressure = single([124 93; 109 77; 125 83; 117 75; 122 80]);
filename = [tempdir(), 'test_writetable_13.csv'];
T = table(LastName, Age, Height, Weight, BloodPressure);
writetable(T, filename, 'WriteRowNames', false)  
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_13.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = complex([38;43;38;40;49], [0.;0.1;-0.2;0.3;0.4]);
Height = [71;69;64;67;64];
Weight = complex([176;163;131;133;119],[0;1;2;3;4]);
BloodPressure = logical([0 1; 1 0; 1 1; 0 0; 1 0]);
filename = [tempdir(), 'test_writetable_14.csv'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', true)  
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_14.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
