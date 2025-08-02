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
[x, y, z] = peaks();
hs = surf (x,y,z);
axis manual
nframes = 50;
mov(nframes) = struct ("cdata", [], "colormap", []);
for ii = 1:nframes
  set (hs, "ZData", z * sin (2*pi*ii/nframes));
  mov(ii) = getframe (f);
end
clf ();
movie (mov, 3, 25);
%=============================================================================
f = figure('Visible', 'off');
axis manual
[X, Y] = meshgrid(linspace(-2, 2, 50)); % Create mesh grid
Z = sin(2 * pi * (X.^2 + Y.^2)); % Initial surface data
h = surf(X, Y, Z); % Plot initial surface
colormap(jet);
colorbar;
title('Animated Surface Plot');

% Number of frames
nFrames = 50;
frames(nFrames) = struct('cdata', [], 'colormap', []);
view([-29    82])
% Animation loop
for k = 1:nFrames
    % Update surface data
    Z = sin(2 * pi * (X.^2 + Y.^2) + 2 * pi * k / nFrames);
    set(h, 'ZData', Z);
    
    % Capture the frame
    frames(k) = getframe(f);
end
close(f);

figure();
movie(frames, 5)
%=============================================================================
