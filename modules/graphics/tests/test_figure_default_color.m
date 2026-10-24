%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A new figure has a light gray background, not white.
f = figure('Visible', 'off', 'Position', [100 100 320 240]);
asserts.isequal(get(f, 'Color'), [0.94 0.94 0.94]);
asserts.isequal(char(get(f, 'InvertHardcopy')), 'on');
%=============================================================================
% plot and surf keep the figure background.
plot(1:3);
drawnow();
asserts.isequal(get(f, 'Color'), [0.94 0.94 0.94]);
surf(peaks());
drawnow();
asserts.isequal(get(f, 'Color'), [0.94 0.94 0.94]);
% the axes stay white on the gray figure.
asserts.isequal(get(gca(), 'Color'), [1 1 1]);
%=============================================================================
% the on-screen frame shows the gray figure background around the axes.
clf(f);
plot(1:3);
drawnow();
frame = getframe(f);
corner = double(reshape(frame.cdata(3, 3, 1:3), 1, 3));
asserts.istrue(all(abs(corner - 0.94 * 255) <= 2));
%=============================================================================
% reset restores the default gray after an explicit Color.
set(f, 'Color', [1 0 0]);
asserts.isequal(get(f, 'Color'), [1 0 0]);
reset(f);
asserts.isequal(get(f, 'Color'), [0.94 0.94 0.94]);
%=============================================================================
% the dark theme replaces the default gray and the light theme brings it back.
theme(f, 'dark');
asserts.istrue(all(get(f, 'Color') < 0.5));
theme(f, 'light');
asserts.isequal(get(f, 'Color'), [0.94 0.94 0.94]);
close(f);
%=============================================================================
