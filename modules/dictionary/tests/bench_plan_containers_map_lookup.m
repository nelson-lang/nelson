%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 3000;
keysToUse = {'k1', 'k2', 'k3', 'k4'};
valuesToUse = [1 2 3 4];
mapValue = containers.Map(keysToUse, valuesToUse);
acc = 0;
tic();
for k = 1:n
  acc = acc + mapValue('k1') + mapValue('k4');
end
elapsed = toc();
ops = n * 2;
asserts.isequal(acc, n * 5);
fprintf('BENCH_REFERENCE,map,containers.Map lookup,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
