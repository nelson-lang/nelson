%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
close all
%=============================================================================
f = figure();
plot(1:3);
ax = gca();
asserts.isequal(char(ax.Box), 'on');
box on
asserts.isequal(char(ax.Box), 'on');
box off
asserts.isequal(char(ax.Box), 'off');
box
asserts.isequal(char(ax.Box), 'on');
box
asserts.isequal(char(ax.Box), 'off');
close(f);
%=============================================================================
f = figure();
ax = axes();
box(ax, 'on');
asserts.isequal(char(ax.Box), 'on');
box(ax, "off");
asserts.isequal(char(ax.Box), 'off');
box(ax, true);
asserts.isequal(char(ax.Box), 'on');
box(ax, 0);
asserts.isequal(char(ax.Box), 'off');
close(f);
%=============================================================================
f = figure();
ax1 = subplot(2, 1, 1);
ax2 = subplot(2, 1, 2);
box([ax1 ax2], 'on');
asserts.isequal(char(ax1.Box), 'on');
asserts.isequal(char(ax2.Box), 'on');
box([ax1 ax2]);
asserts.isequal(char(ax1.Box), 'off');
asserts.isequal(char(ax2.Box), 'off');
close(f);
%=============================================================================
f = figure();
plot(1:3);
lgd = legend({'data'});
box(lgd, 'off');
asserts.isequal(char(lgd.Box), 'off');
box(lgd)
asserts.isequal(char(lgd.Box), 'on');
close(f);
%=============================================================================
f = figure();
imagesc(magic(3));
cb = colorbar();
asserts.isequal(char(cb.Box), 'on');
box(cb, 'off');
asserts.isequal(char(cb.Box), 'off');
box(cb, 1);
asserts.isequal(char(cb.Box), 'on');
close(f);
%=============================================================================
asserts.checkerror('box(''invalid'')', getString(message('Nelson:box:invalidOnOffValue')));
asserts.checkerror('box(2)', getString(message('Nelson:box:invalidOnOffValue')));
asserts.checkerror('box([], ''on'')', getString(message('Nelson:box:invalidFirstArgument')));
%=============================================================================
