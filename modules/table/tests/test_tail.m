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
R = evalc('tail(T, 2)');
REF =  '    LastName     Age    Smoker    Height    Weight    BloodPressure    
    _________    ___    ______    ______    ______    _____________    
                                                                       
    {''Diaz''}     40     false     67        133       117    75        
    {''Brown''}    49     true      64        119       122    80        
 
';
assert_isequal(R, REF);
%=============================================================================
A = repmat((1:100)', 1, 4);
R = evalc('tail(A, 2)');
REF = '    99    99    99    99
   100   100   100   100

';
assert_isequal(R, REF);
%=============================================================================
