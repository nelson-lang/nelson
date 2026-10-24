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
f = figure();
ax = gca();
t1 = title(ax, 'First title');
t2 = title(ax, 'Second title');
asserts.istrue(isgraphics(t1));
asserts.istrue(isgraphics(t2));
asserts.isequal(t1, t2);
asserts.isequal(ax.Title, t2);
%=============================================================================
x1 = xlabel(ax, 'First X label');
x2 = xlabel(ax, 'Second X label');
asserts.istrue(isgraphics(x2));
asserts.isequal(x1, x2);
asserts.isequal(ax.XLabel, x2);
%=============================================================================
y1 = ylabel(ax, 'First Y label');
y2 = ylabel(ax, 'Second Y label');
asserts.istrue(isgraphics(y2));
asserts.isequal(y1, y2);
asserts.isequal(ax.YLabel, y2);
%=============================================================================
z1 = zlabel(ax, 'First Z label');
z2 = zlabel(ax, 'Second Z label');
asserts.istrue(isgraphics(z2));
asserts.isequal(z1, z2);
asserts.isequal(ax.ZLabel, z2);
close all;
%=============================================================================
