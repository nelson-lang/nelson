%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~canUseGPU(), 'No compatible GPU device found.');
%=============================================================================
% end follows the device dimensions
s = gpuArray(single([5; 6; 7]));
assert_isequal(gather(s(end)), single(7));
assert_isequal(gather(s(2:end)), single([6; 7]));
m = gpuArray(single(magic(3)));
assert_isequal(gather(m(end, 1)), single(4));
assert_isequal(gather(m(end)), single(2));
h = reshape(1:24, 2, 3, 4);
z = gpuArray(single(h));
assert_isequal(gather(z(end, end)), single(h(end, end)));
assert_isequal(gather(z(1, 2:end)), single(h(1, 2:end)));
%=============================================================================
% a vector indexed by a vector keeps its orientation; A(:) is a column
c = gpuArray(single([5; 6; 7]));
assert_isequal(size(c(2:3)), [2 1]);
assert_isequal(size(c(1:0)), [0 1]);
r = gpuArray(single([5 6 7]));
assert_isequal(size(r([2; 3])), [1 2]);
assert_isequal(size(r(:)), [3 1]);
assert_isequal(size(m([1 2; 3 1])), [2 2]);
%=============================================================================
% growth and deletion
x = gpuArray(single(1:5));
x(end + 1) = 9;
assert_istrue(isgpuarray(x));
assert_isequal(gather(x), single([1 2 3 4 5 9]));
x(2) = [];
assert_isequal(gather(x), single([1 3 4 5 9]));
w = gpuArray(single(magic(3)));
w(:, 2) = [];
assert_isequal(gather(w), single([8 6; 3 7; 4 2]));
z(2, 12) = 100;
h(2, 12) = 100;
assert_isequal(gather(z), single(h));
assert_checkerror('q = gpuArray(single(magic(3))); q(12) = 1;', _('Attempt to grow array along ambiguous dimension.'));
%=============================================================================
% sorted distinct values and set operations on the device
a = single([3 1 2 3 NaN 1]);
b = single([4 1; 1 NaN]);
assert_istrue(isequaln(gather(unique(gpuArray(a))), unique(a)));
assert_istrue(isequaln(gather(unique(gpuArray(b))), unique(b)));
assert_isequal(gather(unique(gpuArray(single(5)))), single(5));
cases = {a, b, single([3; 1; 2; 3]), single([Inf -Inf 2 Inf]), single([7 8 9])};
names = {'union', 'intersect', 'setdiff', 'setxor', 'ismember'};
for f = 1:numel(names)
  for i = 1:numel(cases)
    for j = 1:numel(cases)
      expected = feval(names{f}, cases{i}, cases{j});
      computed = feval(names{f}, gpuArray(cases{i}), gpuArray(cases{j}));
      assert_istrue(isgpuarray(computed));
      assert_isequal(size(gather(computed)), size(expected));
      assert_istrue(isequaln(double(gather(computed)), double(expected)));
    end
  end
end
assert_isequal(gather(ismember(gpuArray(single([1 Inf NaN 5])), single([Inf 1 NaN]))), [true true false false]);
% other forms keep the host answer
[u, iu] = unique(gpuArray(single([3 1 3])));
assert_isequal(double(gather(iu)), [2; 1]);
[tf, loc] = ismember(gpuArray(single([1 5])), single([5 1 1]));
assert_isequal(double(gather(loc)), [2 1]);
assert_isequal(gather(intersect(gpuArray(single(5)), single([5 6]))), single(5));
%=============================================================================
