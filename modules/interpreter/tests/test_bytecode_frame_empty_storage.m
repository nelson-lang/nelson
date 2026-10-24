%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
savedPath = path();
cleanupPath = onCleanup(@() path(savedPath));
addpath([modulepath('interpreter', 'tests'), '/fibonacci']);
asserts.isequal(fibonacci(25), 75025);
asserts.isequal(fibonacci(single(12)), single(144));
asserts.isequal(full(fibonacci(sparse(12))), 144);

% Growing one empty value must not change its copies or future frame slots.
for repetition = 1:20
  [x, y] = empty_frame_pair(repetition);
  asserts.isequal(x, repetition);
  asserts.isequal(y, zeros(0, 0));
  [a, b] = empty_frame_pair(0);
  a(1) = 77;
  asserts.isequal(a, 77);
  asserts.isequal(b, zeros(0, 0));
  asserts.isequal(y, zeros(0, 0));
end

function [x, y] = empty_frame_pair(n)
  x = [];
  y = x;
  if n > 0
    x(1) = n;
  end
end
