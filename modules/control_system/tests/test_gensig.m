%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
[u, t] = gensig('pulse', 5, 1, -1);
asserts.isequal(u, zeros(0, 1));
asserts.isequal(t, zeros(0, 1));
%=============================================================================
[u, t] = gensig('pulse', 5, 1, 0);
asserts.isequal(u, zeros(0, 1));
asserts.isequal(t, zeros(0, 1));
%=============================================================================
[u, t] = gensig('pulse', 5, 1, 1);
asserts.isequal(u, [1; 0]);
asserts.isequal(t, [0; 1]);
%=============================================================================
[u, t] = gensig('square', 5, 1, 1);
asserts.isequal(u, [0; 0]);
asserts.isequal(t, [0; 1]);
%=============================================================================
[u, t] = gensig('sin', 5, 1, 1);
asserts.isapprox(u, [0; 0.9511], 1e-3);
asserts.isequal(t, [0; 1]);
%=============================================================================
msg = 'The first input argument of the gensig command must be one of the following: ''sin'', ''square'', or ''pulse''.';
id = 'Nelson:control_system:invalidValue:firstInputArgumentSinSquarePulse';
asserts.checkerror('gensig(''cos'', 5, 1, 1)', msg, id);
%=============================================================================
asserts.checkerror('gensig(''tan'', 5, 1, 1)', msg, id);
%=============================================================================
asserts.checkerror('gensig(''sine'', 5, 1, 1)', msg, id);
%=============================================================================
figure();
tau = 3;
Tf = 6;
Ts = 0.1;
%=============================================================================
subplot(3, 1, 1)
[u, t] = gensig("sin", tau, Tf, Ts);
plot(t, u)
title('sin')
%=============================================================================
subplot(3, 1, 2)
[u, t] = gensig("square", tau, Tf, Ts);
plot(t, u)
title('square')
%=============================================================================
subplot(3, 1, 3)
[u, t] = gensig("pulse", tau, Tf, Ts);
plot(t, u)
title('pulse')
%=============================================================================
