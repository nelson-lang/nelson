%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% dot
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T.name = [1; 2; 3];
assert_isequal(T.name, [1; 2; 3]);
%=============================================================================
assert_checkerror('T.name = [1, 2, 3];', _('Value assignment must be same size as existing value.'));
assert_checkerror('T.name = [1; 2; 3; 4];', _('Value assignment must be same size as existing value.'));
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T.name = [];
assert_isequal(T.Properties.VariableNames, {'age'; 'Employed'});
%=============================================================================
% brace
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T{:, 'name'} = {'Ha';'Ma';'St'};
assert_isequal(T.name, {'Ha';'Ma';'St'});
%=============================================================================
assert_checkerror("T{:, 'name'} = {'Ha';'Ma';'St';'f'}", _('Value assignment must be same size as existing value.'));
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T{2:3, 'name'} = {'A1';'A2'};
assert_isequal(T.name, {'Harry';'A1';'A2'});
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
assert_checkerror("T{'A',:}=[]", _('To delete rows or variables by assigning [], use () subscripting instead of {}.'));
%=============================================================================
T = table([1;2;3;4;5],[5;10;15;20;25],[150;300;450;600;750]);
X = T.Var1;
Y = T.Var1(1:3);
T.Var1 = T.Var1 .* 10;
X = T{:,["Var1","Var2"]};
Y = T{1:3,["Var1","Var2"]};
T10 = T{:,["Var1","Var2"]} .* 10;
T{:,["Var1","Var2"]} = T10;
REF = table([100;200;300;400;500],[50;100;150;200;250],[150;300;450;600;750]);
assert_isequal(T, REF);
%=============================================================================
% Parenthese 
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
assert_checkerror("T('name',:)", _('Row name not found.'));
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T.firstName = ["Al"; "Claude"; "Cat"];
R = evalc('T');
REF = ' 
T =
 
  3×4 table
 
         name          age    Employed    firstName    
         __________    ___    ________    _________    
                                                       
    A    {''Harry''}     23     true        "Al"         
    B    {''Mark''}      54     false       "Claude"     
    C    {''Steven''}    13     true        "Cat"        
 
';
assert_isequal(R, REF);
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T('A',:)=[];
name = {'Mark';'Steven'};
age = [54; 13];
Employed = logical([0;1]);
REF = table(name,age, Employed, 'RowNames', {'B';'C'});
assert_isequal(T, REF);
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T({'A','C'},:)=[];
name = {'Mark'};
age = [54];
Employed = logical([0]);
REF = table(name, age, Employed, 'RowNames', {'B'});
assert_isequal(T, REF)
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
T(:,'name') = [];
age = [23; 54; 13];
Employed = logical([1;0;1]);
REF = table(age, Employed, 'RowNames', {'A';'B';'C'});
assert_isequal(T, REF);
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
assert_checkerror("T('A','name')=[]", _("At least one subscript must be ':' when you delete rows or variables by assigning []."));
%=============================================================================
name = {'Harry';'Mark';'Steven'};
age = [23; 54; 13];
Employed = logical([1;0;1]);
T = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
TB = T('B',:);
T('A',:) = TB;
name = {'Mark';'Mark';'Steven'};
age = [54; 54; 13];
Employed = logical([0;0;1]);
T_REF = table(name, age, Employed, 'RowNames', {'A';'B';'C'});
assert_isequal(T, T_REF);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 1 2; 125 83; 117 75; 122 80];
T_REF = table(Age, Height, Weight, BloodPressure);
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(Age, Height, Weight, BloodPressure);
T{2, 4} = [ 1 2];
assert_isequal(T, T_REF);
%=============================================================================
InsectSpecies = {'Monarch Butterfly';'Seven-spot Ladybird';'Orchid Mantis'; 'American Bumblebee';'Blue Dasher Dragonfly'};
InsectOrder = {'Lepidoptera';'Coleoptera';'Mantodea';'Hymenoptera';'Odonata'};
InsectFamily = {'Nymphalidae';'Coccinellidae';'Hymenopodidae'; 'Apidae';'Libellulidae'};
PredatoryInsect = logical([0;1;1;0;1]); 
T = table(InsectOrder,InsectFamily,PredatoryInsect);
T.Properties.RowNames = InsectSpecies;
InsectSpecies = {'bug 1';'bug 2';'bug 3'; 'bug 4';'bug 5'};
T.Properties.RowNames = InsectSpecies;
%=============================================================================

