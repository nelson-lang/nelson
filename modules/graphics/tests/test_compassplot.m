%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
z = [1 + 1i, 1 - 1i];
h = compassplot(z);
asserts.isequal(size(h), [1 1]);
asserts.istrue(isgraphics(h, 'compassplot'));
asserts.isequal(h.Type, 'compassplot');
asserts.isapprox(h.ThetaData(:), [pi / 4; -pi / 4], 1e-12);
asserts.isapprox(h.RData(:), [sqrt(2); sqrt(2)], 1e-12);
asserts.isequal(h(1).Parent.Type, 'polaraxes');
pax = gca();
pax.ThetaAxisUnits = "radians";
asserts.isequal(pax.ThetaAxisUnits, 'radians');
asserts.isapprox(pax.ThetaLim, [0 2 * pi], 1e-12);
asserts.istrue(any(strcmp(pax.ThetaTickLabel, '\pi/2')));
asserts.istrue(any(strcmp(pax.ThetaTickLabel, '\pi')));
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = polaraxes();
h = compassplot(ax, [1 0], [0 1], 'Color', [1 0 0], 'LineWidth', 2);
asserts.isequal(size(h), [1 1]);
asserts.isequal(h(1).Parent, ax);
asserts.isequal(h(1).Color, [1 0 0]);
asserts.isequal(h(1).LineWidth, 2);
asserts.isequal(h(1).ThetaData(:), [1; 0]);
asserts.isequal(h(1).RData(:), [0; 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = compassplot([0 1; 2 3], [1 2; 3 4], 'LineWidth', 2);
asserts.isequal(size(h), [2 1]);
asserts.isequal(h(1).ThetaData(:), [0; 2]);
asserts.isequal(h(1).RData(:), [1; 3]);
asserts.isequal(h(1).Color, [0.0660 0.4430 0.7450]);
asserts.isequal(h(2).ThetaData(:), [1; 3]);
asserts.isequal(h(2).RData(:), [2; 4]);
asserts.isequal(h(2).Color, [0.8660 0.3290 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
tbl = table([0; 1], [2; 3], [1; 2], 'VariableNames', {'theta', 'r1', 'r2'});
h = compassplot(tbl, 'theta', {'r1', 'r2'});
asserts.isequal(size(h), [2 1]);
asserts.isequal(h(1).ThetaData(:), [0; 1]);
asserts.isequal(h(1).RData(:), [2; 3]);
asserts.isequal(h(1).ThetaVariable, 'theta');
asserts.isequal(h(1).RVariable, 'r1');
asserts.isequal(h(2).ThetaData(:), [0; 1]);
asserts.isequal(h(2).RData(:), [1; 2]);
asserts.isequal(h(2).ThetaVariable, 'theta');
asserts.isequal(h(2).RVariable, 'r2');
asserts.istrue(istable(h(1).SourceTable));
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = compassplot([0; 1], [1 2; 3 4]);
asserts.isequal(size(h), [2 1]);
asserts.isequal(h(1).ThetaData(:), [0; 1]);
asserts.isequal(h(2).ThetaData(:), [0; 1]);
close(f);
%=============================================================================
asserts.checkerror('compassplot();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('compassplot({1, 2});', getString(message('Nelson:compassplot:numericVectorDataExpected', 'Value')));
asserts.checkerror('compassplot([1 2], [1 2 3]);', getString(message('Nelson:dimagree:valueValueNumberElements', 'Theta', 'R')));
cmd = ['f = figure(''Visible'', ''off''); ax = axes(); ', ...
  'compassplot(ax, [1 2], [1 2]); close(f);'];
asserts.checkerror(cmd, getString(message('Nelson:graphics:invalidParentInput')));
%=============================================================================
