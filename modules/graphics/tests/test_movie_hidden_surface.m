%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
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
nFrames = 1;
f = figure('Visible', 'off');
axis manual
[X, Y] = meshgrid(linspace(-2, 2, 50));
Z = sin(2 * pi * (X .^ 2 + Y .^ 2));
h = surf(X, Y, Z);
colormap(jet);
colorbar;
title('Animated Surface Plot');
frames(nFrames) = struct('cdata', [], 'colormap', []);
view([-29 82])
for k = 1:nFrames
  Z = sin(2 * pi * (X .^ 2 + Y .^ 2) + 2 * pi * k / nFrames);
  set(h, 'ZData', Z);
  frames(k) = getframe(f);
end
close(f);
figure();
movie(frames, 5)
%=============================================================================
