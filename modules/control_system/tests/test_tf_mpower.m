%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
sys2 = sys ^ 3;
R = evalc('display(sys2)');
REF =   ' 
sys2 =
 
                27 z^3 + 108 z^2 + 144 z + 64
  —————————————————————————————————————————————————————————
  27 z^6 + 27 z^5 + 144 z^4 + 91 z^3 + 240 z^2 + 75 z + 125
 
Sample time: 0.2000 seconds
Discrete-time transfer function.
 
';
assert_isequal(R, REF)
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
sys2 = sys ^ -3;
R = evalc('display(sys2)');
REF = ' 
sys2 =
 
  27 z^6 + 27 z^5 + 144 z^4 + 91 z^3 + 240 z^2 + 75 z + 125
  —————————————————————————————————————————————————————————
                27 z^3 + 108 z^2 + 144 z + 64
 
Sample time: 0.2000 seconds
Discrete-time transfer function.
 
';
assert_isequal(R, REF)
%=============================================================================
