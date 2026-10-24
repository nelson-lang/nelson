%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Concatenation of N-D struct and classdef object arrays keeps the trailing
% dimensions and stores the elements in the same order as a numeric array of
% the same shape.
addpath(modulepath('operators', 'tests'));
makers = {@(x) struct('v', x), ...
          @(x) VertcatOrderValueFixture(x), ...
          @(x) VertcatOrderHandleFixture(x)};
n = reshape(1:8, [1 2 2 2]);
n2 = n + 100;
nc = reshape(1:4, [1 2 2]);
n6 = reshape(1:6, [1 2 3]);
for m = 1:numel(makers)
  mk = makers{m};
  a = mk(n(1));
  b = mk(n2(1));
  for q = 2:numel(n)
    a(q) = mk(n(q));
    b(q) = mk(n2(q));
  end
  a = reshape(a, size(n));
  b = reshape(b, size(n));
  results = {[a; b], [a, b], vertcat(a, b), horzcat(a, b), ...
             cat(1, a, b), cat(2, a, b), cat(3, a, b), cat(4, a, b), cat(5, a, b), ...
             [a; b; a], [a, b, a]};
  expected = {[n; n2], [n, n2], [n; n2], [n, n2], ...
              cat(1, n, n2), cat(2, n, n2), cat(3, n, n2), cat(4, n, n2), cat(5, n, n2), ...
              [n; n2; n], [n, n2, n]};
  for k = 1:numel(results)
    r = results{k};
    assert_isequal(size(r), size(expected{k}));
    values = zeros(size(r));
    for q = 1:numel(r)
      values(q) = r(q).v;
    end
    assert_isequal(values, expected{k});
  end
  %=============================================================================
  c = mk(nc(1));
  c6 = mk(n6(1));
  for q = 2:numel(nc)
    c(q) = mk(nc(q));
  end
  for q = 2:numel(n6)
    c6(q) = mk(n6(q));
  end
  c = reshape(c, size(nc));
  c6 = reshape(c6, size(n6));
  d2 = [mk(1), mk(2)];
  msg = _('Dimensions of arrays being concatenated are not consistent.');
  assert_checkerror('[c; d2]', msg);
  assert_checkerror('[c, d2]', msg);
  assert_checkerror('[d2, c]', msg);
  assert_checkerror('[c; c6]', msg);
  assert_checkerror('[c, c6]', msg);
  assert_checkerror('cat(3, c, [mk(1), mk(2), mk(3)])', msg);
end
%=============================================================================
% Horizontal concatenation checks the trailing dimensions even when the
% numbers of rows agree.
msg = _('Dimensions of arrays being concatenated are not consistent.');
assert_checkerror('[nc, [1 2]]', msg);
assert_checkerror('[[1 2], nc]', msg);
assert_checkerror('[nc, n6]', msg);
assert_checkerror('[n6, nc]', msg);
assert_isequal([nc, nc], cat(2, nc, nc));
assert_isequal(size([nc, nc]), [1 4 2]);
%=============================================================================
