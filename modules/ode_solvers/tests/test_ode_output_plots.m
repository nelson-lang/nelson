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
close all
figure('Visible', 'off');
asserts.isequal(odeplot([0 1], [1; 2], 'init'), 0);
asserts.isequal(odeplot(0.5, [1.5; 2.5], ''), 0);
ax = gca();
asserts.isequal(size(ax.Children), [2 1]);
asserts.isequal(ax.Children(1).XData, [0 0.5]);
asserts.isequal(odeplot([], [], 'done'), 0);
close all
%=============================================================================
figure('Visible', 'off');
asserts.isequal(odephas2([0 1], [1; 2], 'init'), 0);
asserts.isequal(odephas2(0.5, [1.5; 2.5], ''), 0);
ax = gca();
asserts.isequal(size(ax.Children), [1 1]);
asserts.isequal(ax.Children.XData, [1 1.5]);
asserts.isequal(ax.Children.YData, [2 2.5]);
asserts.isequal(odephas2([], [], 'done'), 0);
close all
%=============================================================================
figure('Visible', 'off');
asserts.isequal(odephas3([0 1], [1; 2; 3], 'init'), 0);
asserts.isequal(odephas3(0.5, [1.5; 2.5; 3.5], ''), 0);
ax = gca();
asserts.isequal(size(ax.Children), [1 1]);
asserts.isequal(ax.Children.XData, [1 1.5]);
asserts.isequal(ax.Children.YData, [2 2.5]);
asserts.isequal(ax.Children.ZData, [3 3.5]);
asserts.isequal(odephas3([], [], 'done'), 0);
close all
%=============================================================================
asserts.checkerror('odephas2(0, 1, '''')', _('Not enough solution components.'));
asserts.checkerror('odephas3(0, [1; 2], '''')', _('Not enough solution components.'));
asserts.checkerror('odeplot(0, 1, ''bad'')', _('Unknown output callback flag.'));
%=============================================================================
