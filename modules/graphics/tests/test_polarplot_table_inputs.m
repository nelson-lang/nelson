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
f = figure('Visible', 'off');
Angle = linspace(0, 3 * pi, 50)';
Radius = (1:50)';
tbl = table(Angle, Radius);
p = polarplot(tbl, "Angle", "Radius");
asserts.isequal(p.Type, 'line');
asserts.isequal(p.ThetaData(:), Angle);
asserts.isequal(p.RData(:), Radius);
asserts.isequal(p.ThetaVariable, 'Angle');
asserts.isequal(p.RVariable, 'Radius');
asserts.isequal(rlim(), [0 50]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
Radius1 = Radius;
Radius2 = Radius1 / 2;
tbl = table(Angle, Radius1, Radius2);
p = polarplot(tbl, "Angle", ["Radius1" "Radius2"]);
asserts.isequal(size(p), [2 1]);
asserts.isequal(p(1).ThetaData(:), Angle);
asserts.isequal(p(1).RData(:), Radius1);
asserts.isequal(p(2).ThetaData(:), Angle);
asserts.isequal(p(2).RData(:), Radius2);
asserts.isequal(p(1).ThetaVariable, 'Angle');
asserts.isequal(p(2).ThetaVariable, 'Angle');
asserts.isequal(p(1).RVariable, 'Radius1');
asserts.isequal(p(2).RVariable, 'Radius2');
asserts.isequal(p(1).DisplayName, 'Radius1');
asserts.isequal(p(2).DisplayName, 'Radius2');
asserts.isequal(rlim(), [0 50]);
L = findobj(f, 'Tag', 'legend');
asserts.isequal(numel(L), 1);
asserts.istrue(isgraphics(L, 'legend'));
asserts.isequal(L.String, {'Radius1'; 'Radius2'});
asserts.istrue(L.Position(4) < 0.2);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes('Parent', f);
p = polarplot(ax, table(Angle, Radius), "Angle", "Radius", 'o-', 'LineWidth', 2);
asserts.isequal(p.Parent, ax);
asserts.isequal(p.LineStyle, '-');
asserts.isequal(p.Marker, 'o');
asserts.isequal(p.LineWidth, 2);
asserts.isequal(p.ThetaData(:), Angle);
asserts.isequal(p.RData(:), Radius);
close(f);
%=============================================================================
