%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = evalc('T = table()');
REF = ' 
T =
 
  0×0 table
 
';
assert_isequal(R, REF);
%=============================================================================
R = evalc('T = table(1)');
REF = ' 
T =
 
  1×1 table
 
    Var1    
    ____    
            
    1       
 
';
assert_isequal(R, REF);
%=============================================================================
A = 1;
R = evalc('T = table(A)');
REF = ' 
T =
 
  1×1 table
 
    A    
    _    
         
    1    
 
';
assert_isequal(R, REF);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
T2 = table(LastName, Age, Smoker, Height, Weight, BloodPressure, 'RowNames', {'A1';'A2';'A3';'A4';'A5'});
R = evalc('T1');
REF =  ' 
T1 =
 
  5×6 table
 
    LastName       Age    Smoker    Height    Weight    BloodPressure    
    ___________    ___    ______    ______    ______    _____________    
                                                                         
    {''Sanchez''}    38     true      71        176       124    93        
    {''Johnson''}    43     false     69        163       109    77        
    {''Li''}         38     true      64        131       125    83        
    {''Diaz''}       40     false     67        133       117    75        
    {''Brown''}      49     true      64        119       122    80        
 
';
assert_isequal(R, REF);
%=============================================================================
R = evalc('T2');
REF =  ' 
T2 =
 
  5×6 table
 
          LastName       Age    Smoker    Height    Weight    BloodPressure    
          ___________    ___    ______    ______    ______    _____________    
                                                                               
    A1    {''Sanchez''}    38     true      71        176       124    93        
    A2    {''Johnson''}    43     false     69        163       109    77        
    A3    {''Li''}         38     true      64        131       125    83        
    A4    {''Diaz''}       40     false     67        133       117    75        
    A5    {''Brown''}      49     true      64        119       122    80        
 
';
assert_isequal(R, REF);
%=============================================================================
LastName = ["Sanchez";"Johnson";"Li";"Diaz";"Brown"];
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
R = evalc('T1');
REF = ' 
T1 =
 
  5×6 table
 
    LastName     Age    Smoker    Height    Weight    BloodPressure    
    _________    ___    ______    ______    ______    _____________    
                                                                       
    "Sanchez"    38     true      71        176       124    93        
    "Johnson"    43     false     69        163       109    77        
    "Li"         38     true      64        131       125    83        
    "Diaz"       40     false     67        133       117    75        
    "Brown"      49     true      64        119       122    80        
 
';
assert_isequal(R, REF);
%=============================================================================
T = table(ones(2, 1, 3),2*ones(2,2,3),3*ones(2,3,3),'VariableNames',["One" "Two" "Three"]);
R = evalc('T');
REF =  ' 
T =
 
  2×3 table
 
    One             Two             Three           
    ____________    ____________    ____________    
                                                    
    1x1x3 double    1x2x3 double    1x3x3 double    
    1x1x3 double    1x2x3 double    1x3x3 double    
 
';
assert_isequal(R, REF);
%=============================================================================