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
% Deleting an object parented to the first figure (internal id 0) must drop it from the figure
% children. A stale child handle was recycled later and deleted by mistake with its figure.
%=============================================================================
close all
for k = 1:6
  f(k) = figure('Visible', 'off');
  h = heatmap(rand(6 + k));
  h.ColorbarVisible = 'off';
  delete(h);
end
x = heatmap(rand(3));
delete(f(1));
asserts.istrue(isgraphics(x.UserData.Grid));
asserts.istrue(isgraphics(x.UserData.Image));
asserts.istrue(isgraphics(x));
delete(f(6));
asserts.istrue(~isgraphics(x));
close all
%=============================================================================
f1 = figure('Visible', 'off');
ax = axes(f1);
cb = colorbar(ax);
delete(cb);
other = figure('Visible', 'off');
ax2 = axes(other);
p = plot(ax2, 1:3);
close(f1);
asserts.istrue(isgraphics(p));
asserts.istrue(isgraphics(ax2));
close all
%=============================================================================
