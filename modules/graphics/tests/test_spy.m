%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
S = sparse(rand(1, 10) * 100 + 1, rand(1, 10) * 100 + 1, rand(1, 10) * 100);
spy(S);
close(f);
%=============================================================================
f = figure();
spy();
close(f);
%=============================================================================
out = tempname();
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460], 'Color', [0.94 0.94 0.94]);
A = magic(5);
[L, U, P] = lu(A);
subplot(1, 2, 1);
spy(L);
title('L factor');
subplot(1, 2, 2);
spy(U);
title('U factor');
drawnow();
img = getframe(f).cdata;
imwrite(img, [out, '/spy_subplot.png']);

% getframe returns device pixels: scale the sample points accordingly
r = f.DevicePixelRatio;
pixelAt = @(row, col) double(reshape(img(round(row * r), round(col * r), :), 1, 3));
% The plot box samples sit in the half of the box its triangular factor leaves
% empty -- to the right of the diagonal for L, to the left of it for U. A spy
% marker is wide enough that a sample taken in the filled half is a coin toss.
leftTopMargin = pixelAt(80, 190);
leftPlotBox = pixelAt(230, 250);
rightTopMargin = pixelAt(80, 560);
rightPlotBox = pixelAt(230, 400);

asserts.istrue(max(leftTopMargin) < 250);
asserts.istrue(min(leftPlotBox) >= 250);
asserts.istrue(max(rightTopMargin) < 250);
asserts.istrue(min(rightPlotBox) >= 250);
close(f);
rmdir(out, 's');
%=============================================================================
