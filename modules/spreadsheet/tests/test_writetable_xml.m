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
filename = [tempdir(), 'test_writetable_1.xml'];
writetable(T, filename);
R = fileread(filename);
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_1.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45 45;41 32;40 34],{'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_2.xml'];
writetable(T, filename);
R = fileread(filename);
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_2.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
filename = [tempdir(), 'test_writetable_3.xml'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', false)  
R = fileread(filename);    
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_3.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
filename = [tempdir(), 'test_writetable_4.xml'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', true)  
R = fileread(filename);    
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_4.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
T = table(['M';'F';'M'],[45 45;41 32;40 34],{'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_5.xml'];
writetable(T, filename, 'WriteVariableNames', false);
R = fileread(filename);
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_5.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
NNN_1 =  ['M';'F';'M'];
T = table(NNN_1,[45 45;41 32;40 34],{'NY';'CA';'MA'},[true;false;false]);
filename = [tempdir(), 'test_writetable_6.xml'];
writetable(T, filename, 'AttributeSuffix', 'toto');
R = fileread(filename);
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_6.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
InsectSpecies = {'Monarch Butterfly';'Seven-spot Ladybird';'Orchid Mantis'; 'American Bumblebee';'Blue Dasher Dragonfly'};
InsectOrder = {'Lepidoptera';'Coleoptera';'Mantodea';'Hymenoptera';'Odonata'};
InsectFamily = {'Nymphalidae';'Coccinellidae';'Hymenopodidae'; 'Apidae';'Libellulidae'};
PredatoryInsect = logical([0;1;1;0;1]); 
T = table(InsectOrder,InsectFamily,PredatoryInsect);
T.Properties.RowNames = InsectSpecies;
InsectSpecies = {'bug 1';'bug 2';'bug 3'; 'bug 4';'bug 5'};
T.Properties.RowNames = InsectSpecies;
filename = [tempdir(), 'test_writetable_7.xml'];
writetable(T, filename, "WriteRowNames", true, "RowNodeName", "Insect")
R = fileread(filename);
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_7.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = [38;43;38;40;49];
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = logical([0 1; 1 0; 1 1; 0 0; 1 0]);
filename = [tempdir(), 'test_writetable_8.xml'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', false)  
R = fileread(filename);    
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_8.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
data = table([1; 2; 3], {'A'; 'B'; 'C'}, [10.5; 20.7; 30.2], 'VariableNames', {'ID', 'Name', 'Value'});
data.Value_Attribute = {'High'; 'Medium'; 'Low'};
filename = [tempdir(), 'test_writetable_9.xml'];
writetable(data, filename, 'AttributeSuffix', '_Attribute')
R = fileread(filename);    
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_9.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
data = table([1; 2; 3], {'A'; 'B'; 'C'}, [10.5; 20.7; 30.2], 'VariableNames', {'ID', 'Name', 'Value'});
data.Value_Attribute = {'High'; 'Medium'; 'Low'};
data.Properties.RowNames = {'bug 1';'bug 2';'bug 3'};
filename = [tempdir(), 'test_writetable_10.xml'];
writetable(data, filename, 'AttributeSuffix', '_Attribute', 'WriteRowNames', true)
R = fileread(filename);    
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_10.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================
LastName = {'Smith';'Johnson';'Williams';'Jones';'Brown'};
Age = complex([38;43;38;40;49], [0.;0.1;-0.2;0.3;0.4]);
Height = [71;69;64;67;64];
Weight = complex([176;163;131;133;119],[0;1;2;3;4]);
BloodPressure = logical([0 1; 1 0; 1 1; 0 0; 1 0]);
filename = [tempdir(), 'test_writetable_11.xml'];
T = table(Age,Height,Weight,BloodPressure,  'RowNames',LastName);
writetable(T, filename, 'WriteRowNames', true)  
R = fileread(filename);
xml_ref_filename = [modulepath('spreadsheet'), '/tests/test_writetable_11.xml'];
REF =  fileread(xml_ref_filename);
assert_isequal(R, REF);
%=============================================================================