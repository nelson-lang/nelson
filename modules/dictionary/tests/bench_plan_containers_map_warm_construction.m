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
tic();
for k = 1:n
  mapValue = containers.Map(keysToUse, valuesToUse);
end
elapsed = toc();
ops = n;
asserts.isequal(mapValue('k3'), 3);
fprintf('BENCH_REFERENCE,map,containers.Map warm construction,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
