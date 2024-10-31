%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
LastName = {'Sanchez';'Johnson';'Li';'Diaz';'Brown'};
Age = [38;43;38;40;49];
Smoker = logical([1;0;1;0;1]);
Height = [71;69;64;67;64];
Weight = [176;163;131;133;119];
BloodPressure = [124 93; 109 77; 125 83; 117 75; 122 80];
T = table(LastName, Age, Smoker, Height, Weight, BloodPressure);
R = evalc('head(T, 2)');
REF =    '    LastName       Age    Smoker    Height    Weight    BloodPressure    
    ___________    ___    ______    ______    ______    _____________    
                                                                         
    {''Sanchez''}    38     true      71        176       124    93        
    {''Johnson''}    43     false     69        163       109    77        
 
';
assert_isequal(R, REF);
%=============================================================================
A = repmat((1:100)',1,4);
R = evalc('head(A)');
REF =  '     1     1     1     1
     2     2     2     2
     3     3     3     3
     4     4     4     4
     5     5     5     5
     6     6     6     6
     7     7     7     7
     8     8     8     8

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('head(A, 2)');
REF =   '     1     1     1     1
     2     2     2     2

';
assert_isequal(R, REF); 
%=============================================================================
R = head(A, 8);
REF = repmat((1:8)', 1, 4);
assert_isequal(R, REF);
%=============================================================================
msg = sprintf(_('Invalid input argument at position %d.\nValue must be greater than or equal to %d.'), 2, 0);
assert_checkerror('head(A, -1)', msg, 'Nelson:validators:mustBeGreaterThanOrEqual');
%=============================================================================
msg = sprintf(_('Invalid input argument at position %d.\nValue must be integer.'), 2);
assert_checkerror('head(A, 1.2)', msg);
%=============================================================================
