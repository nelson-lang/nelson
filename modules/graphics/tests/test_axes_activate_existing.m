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
% axes(ax) makes its figure current and visible
f1 = figure('Visible', 'off');
ax1 = axes(f1);
f2 = figure('Visible', 'off');
assert_istrue(gcf() == f2);
axes(ax1);
assert_istrue(gcf() == f1);
assert_istrue(strcmp(f1.Visible, 'on'));
assert_istrue(strcmp(f2.Visible, 'off'));
assert_istrue(f1.CurrentAxes == ax1);
%=============================================================================
% axes in a panel of another figure
f3 = figure('Visible', 'off');
p = uipanel(f3);
ax3 = axes(p);
figure(f2);
axes(ax3);
assert_istrue(gcf() == f3);
assert_istrue(strcmp(f3.Visible, 'on'));
assert_istrue(f3.CurrentAxes == ax3);
%=============================================================================
% polar axes
f4 = figure('Visible', 'off');
pax = polaraxes(f4);
figure(f2);
axes(pax);
assert_istrue(gcf() == f4);
assert_istrue(strcmp(f4.Visible, 'on'));
%=============================================================================
% activating existing axes returns no output
assert_checkerror('r = axes(ax1);', _('Too many output arguments.'), 'Nelson:tooManyOutputs');
assert_checkerror('r = axes(pax);', _('Too many output arguments.'), 'Nelson:tooManyOutputs');
%=============================================================================
% a container argument still creates and returns new axes
ax5 = axes(f2);
assert_istrue(isgraphics(ax5, 'axes'));
assert_istrue(ax5.Parent == f2);
%=============================================================================
close all
%=============================================================================
