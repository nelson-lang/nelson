%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
x = linspace(0,pi);
y1 = cos(x);
plot(x, y1)
hold on 
y2 = cos(2*x);
plot(x, y2)
L = legend('cos(x)','cos(2x)')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'N')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'S')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'W')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'E')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'NE')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'NW')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'SE')
pause(0.2);
legend('off')
legend('cos(x)','cos(2x)', 'location', 'SW')
pause(0.2);
legend('off')
%=============================================================================
