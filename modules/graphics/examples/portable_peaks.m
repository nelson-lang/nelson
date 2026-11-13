%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Portable plotting demo shared by native and WebAssembly builds.
x = linspace(0, 2 * pi, 160);
figure('Name', 'Portable sine curve', 'NumberTitle', 'off');
plot(x, sin(x), 'LineWidth', 1.5);
grid on;
xlim([0, 2 * pi]);
title('y = sin(x)');
xlabel('x (rad)');
ylabel('y');

figure('Name', 'Portable peaks surface', 'NumberTitle', 'off');
surf(peaks(49));
title('peaks(49) surface - drag to rotate');
xlabel('column');
ylabel('row');
zlabel('height');
