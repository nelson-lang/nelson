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
f = figure('Visible', 'off');
h = streamtube(verts);
asserts.isequal(numel(h), 1);
asserts.istrue(isgraphics(h(1), 'surface'));
asserts.isequal(h(1).Type, 'surface');
asserts.isequal(size(h(1).XData), [numel(t) 21]);
asserts.isequal(size(h(1).YData), [numel(t) 21]);
asserts.isequal(size(h(1).ZData), [numel(t) 21]);
asserts.isequal(h(1).FaceColor, 'flat');
asserts.isapprox(h(1).EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = streamtube(verts, 0.2);
asserts.istrue(isgraphics(h(1), 'surface'));
close(f);
%=============================================================================
asserts.checkerror('streamtube({[1 2]});', getString(message('Nelson:graphics:verticesNotNumericArrays')));
%=============================================================================
