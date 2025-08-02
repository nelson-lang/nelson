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
n = 300;
N = 200;
x = linspace(-1, 1, N);
y = linspace(-1.4, .6, N);
[X,Y] = meshgrid(x, y);
Z = X + i*Y;
Zn = Z;
for j=1:n
  Zn = -i*(Zn).^2 + (rand(N,N).^(1/5)).*Z;
  M = abs(Zn);
  ind1 = find(M<2);
  ind2 = find(M>=2);
  M(ind1) = 0;
  M(ind2) = -1;
end
im = imagesc(x, y, M);
assert_isequal(length(im.XData), 200);
assert_isequal(length(im.YData), 200); 
colormap([1 1 1; 1 0 0]);
ax = gca();
assert_isequal(ax.YDir, 'reverse');
ax.YDir = 'normal';
assert_isequal(ax.YDir, 'normal');
%=============================================================================
