%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 32-->
% <--REUSE PROCESS-->
%=============================================================================
rng(0);
tables = randi(80, 400, 4);
tic();
for k = 1:size(tables, 1)
  x = reshape(tables(k, :), [2 2]);
  [h, p, stats] = fishertest(x);
end
t = toc();
asserts.istrue(islogical(h));
asserts.istrue(isfinite(p) && p >= 0 && p <= 1);
asserts.istrue(isfield(stats, 'OddsRatio'));
asserts.istrue(isfield(stats, 'ConfidenceInterval'));
asserts.istrue(t < 10);
%=============================================================================
x = [400 130; 260 210];
tic();
for k = 1:200
  [h2, p2, stats2] = fishertest(x, 'Tail', 'right');
end
t2 = toc();
asserts.istrue(islogical(h2));
asserts.istrue(isfinite(p2) && p2 >= 0 && p2 <= 1);
asserts.istrue(isfinite(stats2.OddsRatio));
asserts.istrue(t2 < 10);
%=============================================================================
