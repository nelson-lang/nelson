%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
numerator = [1, 0, 0];
denominator = [1, 2, 3];
ts = 0.1;
sys1 = tf(numerator, denominator, ts);
R = evalc('display(sys1)');
REF = ' 
sys1 =
 
       z^2
  —————————————
  z^2 + 2 z + 3
 
Sample time: 0.1000 seconds
Discrete-time transfer function.
 
';
asserts.isequal(R, REF)
%=============================================================================
sys2 = sys1;
sys2.Variable = 'z^-1';
R = evalc('display(sys2)');
REF =  ' 
sys2 =
 
           1
  ———————————————————
  1 + 2 z^-1 + 3 z^-2
 
Sample time: 0.1000 seconds
Discrete-time transfer function.
 
';
asserts.isequal(R, REF)
%=============================================================================
