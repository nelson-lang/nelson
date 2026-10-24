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
close all
reset(groot());
%=============================================================================
% warp(X, Y, Z, A) maps a truecolor image on the surface and returns a surface
% handle configured for texture mapping.
f = figure('Visible', 'off');
[X, Y] = meshgrid(linspace(-1, 1, 20), linspace(-1, 1, 20));
Z = 1 - (X .^ 2 + Y .^ 2);
[gx, gy] = meshgrid(linspace(0, 1, 48), linspace(0, 1, 32));
A = zeros(32, 48, 3);
A(:, :, 1) = gx;
A(:, :, 2) = gy;
h = warp(X, Y, Z, A);
asserts.istrue(isgraphics(h, 'surface'));
asserts.isequal(char(h.FaceColor), 'texturemap');
asserts.isequal(char(h.EdgeColor), 'none');
asserts.isequal(char(h.CDataMapping), 'direct');
asserts.isequal(size(h.CData), [32 48 3]);
asserts.isequal(size(h.ZData), [20 20]);
ax = ancestor(h, 'axes');
asserts.isequal(char(ax.YDir), 'reverse');
asserts.isequal(ax.View, [-37.5 30]);
close(f);
%=============================================================================
% warp(Z, A) builds the X/Y grid from the Z size.
f = figure('Visible', 'off');
Zp = peaks(25);
h = warp(Zp, A);
asserts.isequal(size(h.ZData), [25 25]);
asserts.isequal(size(h.CData), [32 48 3]);
asserts.isequal(char(h.FaceColor), 'texturemap');
close(f);
%=============================================================================
% warp(Z, A, map): indexed image with a colormap, CDataMapping stays 'direct'.
f = figure('Visible', 'off');
idx = uint8(repmat([1 1 2 2 3 3 4 4], 8, 1));
cmap = [1 0 0; 0 1 0; 0 0 1; 1 1 0];
h = warp(zeros(8, 8), idx, cmap);
asserts.isequal(char(h.CDataMapping), 'direct');
asserts.isequal(size(h.CData), [8 8]);
asserts.isequal(ancestor(h, 'axes').Colormap, cmap);
close(f);
%=============================================================================
% warp(X, Y, Z, A, map).
f = figure('Visible', 'off');
h = warp(X, Y, Z, idx, cmap);
asserts.isequal(char(h.FaceColor), 'texturemap');
asserts.isequal(ancestor(h, 'axes').Colormap, cmap);
close(f);
%=============================================================================
% Input validation.
assertWarpError('warp(zeros(4, 4), [])', 'Nelson:warp:invalidImage');
assertWarpError('warp([1 2 3], [1 2], zeros(4, 4), ones(4, 4, 3))', 'Nelson:warp:invalidSizeZ');
close all
%=============================================================================
function assertWarpError(command, identifier)
  try
    eval(command);
    asserts.fail([command, ' did not fail.']);
  catch ME
    asserts.isequal(ME.identifier, identifier);
  end
end
%=============================================================================
