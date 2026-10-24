%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: growth loops executed at script level (workspace variables).
% The first textual indexed assignment of a script uses the same dedicated
% in-place opcodes as function locals, so appends stay amortized instead of
% copying the whole base on every iteration.
N = 50000;
%=============================================================================
tic();
for i = 1:N
  benchScriptCell{i} = i; %#ok<SAGROW>
end
t = toc();
asserts.isequal(size(benchScriptCell), [1 N]);
asserts.isequal(benchScriptCell{N}, N);
fprintf('  script cell growth, %d appends:   %0.4f s\n', int32(N), t);
%=============================================================================
tic();
for i = 1:N
  benchScriptVec(i) = i; %#ok<SAGROW>
end
t = toc();
asserts.isequal(size(benchScriptVec), [1 N]);
asserts.isequal(benchScriptVec(N), N);
fprintf('  script vector growth, %d appends: %0.4f s\n', int32(N), t);
%=============================================================================
tic();
for i = 1:N
  benchScriptStruct(i).Id = i; %#ok<SAGROW>
end
t = toc();
asserts.isequal(size(benchScriptStruct), [1 N]);
asserts.isequal(benchScriptStruct(N).Id, N);
fprintf('  script struct growth, %d appends: %0.4f s\n', int32(N), t);
%=============================================================================
