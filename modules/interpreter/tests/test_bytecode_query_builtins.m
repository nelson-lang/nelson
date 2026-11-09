%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% numel, isempty, length, ndims and size(x, d) called in a loop give the
% builtin results for every builtin class, and a variable of the same name
% still takes precedence.
%=============================================================================
values = {[], zeros(1, 0), zeros(3, 0, 2), 5, 1:4, (1:4)', magic(3), rand(2, 3, 4), ...
  int8([1 2 3]), single(ones(2, 2)), true(1, 5), 'abc', '', {1, 2, 3}, cell(2, 0), ...
  struct('a', {1, 2}), struct([]), ["a", "b"; "c", "d"], sparse([1 0; 0 2]), 1 + 2i, ...
  zeros(2, 1, 1, 1)};
for k = 1:numel(values)
  x = values{k};
  [r, s] = queries(x);
  assert_isequal(r, {feval('numel', x), feval('isempty', x), feval('length', x), ...
    feval('ndims', x)});
  assert_isequal(s, [feval('size', x, 1), feval('size', x, 2), feval('size', x, 3), ...
    feval('size', x, 4)]);
  assert_isequal(class(r{2}), 'logical');
end
%=============================================================================
% a local or context variable of the same name is indexed instead
assert_isequal(shadowed_numel(), [10 20]);
assert_isequal(shadowed_after_eval(), [3 30]);
%=============================================================================
% an invalid dimension keeps the builtin error
assert_checkerror('size_of(magic(3), 0)', ...
  'Dimension argument must be a positive integer scalar within indexing range.');
assert_checkerror('size_of(magic(3), 1.5)', ...
  'Dimension argument must be a positive integer scalar within indexing range.');
assert_isequal(size_of(magic(3), 'r'), 3);
%=============================================================================
function [r, s] = queries(x)
  r = cell(1, 4);
  s = zeros(1, 4);
  for k = 1:2
    r = {numel(x), isempty(x), length(x), ndims(x)};
    s = [size(x, 1), size(x, 2), size(x, 3), size(x, 4)];
  end
end
%=============================================================================
function r = shadowed_numel()
  numel = [10 20 30];
  r = zeros(1, 2);
  for k = 1:2
    r(k) = numel(k);
  end
end
%=============================================================================
function r = shadowed_after_eval()
  v = 1:3;
  r = zeros(1, 2);
  r(1) = length(v);
  eval('length = [30 40];'); %#ok<NLS0022,NLS0034>
  r(2) = length(1);
end
%=============================================================================
function d = size_of(x, dim)
  d = 0;
  for k = 1:2
    d = size(x, dim);
  end
end
%=============================================================================
