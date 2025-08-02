%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = 1;
B = 2;
T1 = table(A);
T2 = table(B);
T = horzcat(T1, T2);
REF = table(A, B);
assert_isequal(T, REF);
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T1 = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
LastName = {'Simpson';'Biden';'Lewis'};
Age = [37;42;31];
Smoker = logical([1;1;1]);
Height = [70;71;65];
Weight = [171;164;132];
BloodPressure = [121 93; 101 77; 124 83];
T2 = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
R = evalc('T3 = [T1;T2]');
REF = ' 
T3 =
 
  8×6 table
 
    LastName       Age    Smoker    Height    Weight    BloodPressure    
    ___________    ___    ______    ______    ______    _____________    
                                                                         
    {''Sanchez''}    38     true      71        176       124    93        
    {''Johnson''}    43     false     69        163       109    77        
    {''Li''}         38     true      64        131       125    83        
    {''Diaz''}       40     false     67        133       117    75        
    {''Brown''}      49     true      64        119       122    80        
    {''Simpson''}    37     true      70        171       121    93        
    {''Biden''}      42     true      71        164       101    77        
    {''Lewis''}      31     true      65        132       124    83        
 
';
assert_isequal(R, REF);
%=============================================================================
HeartRate = [72; 80; 75; 78; 69; 83; 69; 71];
KnownAllergies = {'None'; 'Peanuts'; 'None'; 'Penicillin'; 'None'; 'Fish'; 'None'; 'Apple'}; 
T4  = table(HeartRate, KnownAllergies);
R = evalc('T5 = [T3, T4]');
REF =  ' 
T5 =
 
  8×8 table
 
    LastName       Age    Smoker    Height    Weight    BloodPressure    HeartRate    KnownAllergies    
    ___________    ___    ______    ______    ______    _____________    _________    ______________    
                                                                                                        
    {''Sanchez''}    38     true      71        176       124    93        72           {''None''}          
    {''Johnson''}    43     false     69        163       109    77        80           {''Peanuts''}       
    {''Li''}         38     true      64        131       125    83        75           {''None''}          
    {''Diaz''}       40     false     67        133       117    75        78           {''Penicillin''}    
    {''Brown''}      49     true      64        119       122    80        69           {''None''}          
    {''Simpson''}    37     true      70        171       121    93        83           {''Fish''}          
    {''Biden''}      42     true      71        164       101    77        69           {''None''}          
    {''Lewis''}      31     true      65        132       124    83        71           {''Apple''}         
 
';
assert_isequal(R, REF);
%=============================================================================
