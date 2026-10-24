%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
[X, Y] = meshgrid(1:256, 1:256);
A = mod(17 * X + 31 * Y, 257) / 257;
A(32:64:end, 48:64:end) = 1;
U = uint8(mod(13 * X + 19 * Y, 256));
L = mod(X + 2 * Y, 7) < 3;
timerStart = tic();
for k = 1:3
  B3 = medfilt2(A, [3 3], 'symmetric');
  B5 = medfilt2(A, [5 5], 'replicate');
  U5 = medfilt2(U, [5 5], 'symmetric');
  L7 = medfilt2(L, [7 7], 'replicate');
end
elapsed = toc(timerStart);
ip_bench_report('medfilt2_mixed_256_three_passes', elapsed, 3.0);
asserts.isequal(size(B3), size(A));
asserts.isequal(size(B5), size(A));
asserts.isequal(size(U5), size(U));
asserts.isequal(size(L7), size(L));
asserts.istrue(all(isfinite(B3(:))));
asserts.istrue(all(isfinite(B5(:))));
asserts.isequal(class(U5), 'uint8');
asserts.isequal(class(L7), 'logical');
clear X Y A U L B3 B5 U5 L7 k timerStart elapsed;
%=============================================================================
