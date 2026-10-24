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
out = tempname();
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
[X, Y] = meshgrid(-2:0.5:2, -2:0.5:2);
U = -Y;
V = X;
quiver(X, Y, U, V);
axis equal;
drawnow();
img = getframe(f).cdata;
imwrite(img, [out, '/quiver_axis_equal.png']);

asserts.istrue(min(double(reshape(img(120, 310, :), 1, 3))) >= 250);
asserts.istrue(min(double(reshape(img(230, 310, :), 1, 3))) >= 250);
close(f);
rmdir(out, 's');
%=============================================================================
