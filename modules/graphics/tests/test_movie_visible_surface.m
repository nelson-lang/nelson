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
nframes = 5;
f = figure();
[x, y, z] = peaks();
hs = surf(x, y, z);
axis manual
mov(nframes) = struct("cdata", [], "colormap", []);
for ii = 1:nframes
  set(hs, "ZData", z * sin(2 * pi * ii / nframes));
  mov(ii) = getframe(f);
end
clf();
movie(mov, 3, 25);
%=============================================================================
