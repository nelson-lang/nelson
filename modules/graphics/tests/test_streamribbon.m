%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
t = 0:.15:2;
verts = {[cos(t)' sin(t)' t']};
twistangle = {cos(t)'};
f = figure('Visible', 'off');
h = streamribbon(verts, twistangle);
asserts.isequal(numel(h), 1);
asserts.istrue(isgraphics(h(1), 'surface'));
asserts.isequal(h(1).Type, 'surface');
asserts.isequal(size(h(1).XData), [numel(t) 2]);
asserts.isequal(size(h(1).YData), [numel(t) 2]);
asserts.isequal(size(h(1).ZData), [numel(t) 2]);
asserts.isequal(size(h(1).CData), [numel(t) 2]);
asserts.isequal(h(1).FaceColor, 'flat');
asserts.isapprox(h(1).EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
asserts.isapprox(h(1).XData(1:3, :), ...
  [1.04230261147853 0.957697388521471; ...
   1.03215015169926 0.945392004172828; ...
   0.962116037089351 0.948556941161861], 1e-12);
asserts.isapprox(h(1).YData(1:3, :), ...
  [-0.0155076094147726 0.0155076094147726; ...
   0.16673602171035 0.132140243236849; ...
   0.330851994442848 0.260188418879831], 1e-12);
ax = gca();
asserts.isapprox(ax.XLim, [-0.4 1.2], 1e-12);
asserts.isapprox(ax.XTick, -0.4:0.2:1.2, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = streamribbon(verts, twistangle, 0.2);
asserts.istrue(isgraphics(h(1), 'surface'));
close(f);
%=============================================================================
asserts.checkerror('streamribbon({[1 2]}, {[0]});', getString(message('Nelson:graphics:verticesNotNumericArrays')));
asserts.checkerror('streamribbon(verts, {[0 1]});', getString(message('Nelson:streamribbon:twistAnglesCountMismatch')));
%=============================================================================
