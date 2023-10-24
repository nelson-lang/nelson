%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/Nelson-numerical-software/nelson/issues/975
% <-- Short Description -->
% Legend color is not matching that of curve in figure.
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
a = [0:0.5:5];
b = 2*a.^2 + 3*a -5;
c = 1.2*a.^2+4*a-3;
subplot(1,2,1)
plot(a,b,'-or','MarkerFaceColor','g','LineWidth',2)
xlabel('X'); ylabel('Y'); legend('Curve ','Location','NorthWest')
subplot(1,2,2)
plot(a,c,'--ok','MarkerFaceColor','c','LineWidth',2)
xlabel('X'); ylabel('Y'); legend('Curve 2','Location','NorthWest')
%=============================================================================
f = figure();
a = [0:0.5:5];
b = 2*a.^2 + 3*a -5;
plot(a,b,'-or','MarkerFaceColor','g','LineWidth', 3);
legend('Curve ','Location','NorthWest')
childrens = f.Children;
axe_legend = childrens(2);
line_legend = axe_legend.Children(2);
line_legend.Color = 'red';
assert_isequal(line_legend.Color, [1 0 0]);
assert_isequal(line_legend.LineWidth, 3);
%=============================================================================
