%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% shape predicates of duration, datetime and calendarDuration arrays match
% the ones of a numeric array of the same size
shapes = {[1 1], [1 2], [2 1], [2 3], [0 0], [1 0], [0 3], [2 3 2], [1 1 3]};
makers = {@(sz) seconds(zeros(sz)), ...
          @(sz) datetime(2020, 1, 1) + days(zeros(sz)), ...
          @(sz) calmonths(zeros(sz))};
predicates = {@isscalar, @isvector, @isrow, @iscolumn, @ismatrix, @isempty, ...
              @numel, @length, @ndims};
for m = 1:numel(makers)
  for s = 1:numel(shapes)
    sz = shapes{s};
    obj = makers{m}(sz);
    ref = zeros(sz);
    for p = 1:numel(predicates)
      assert_isequal(predicates{p}(obj), predicates{p}(ref));
    end
    assert_isequal(size(obj), size(ref));
    assert_isequal(size(obj, 1), size(ref, 1));
    assert_isequal(size(obj, 2), size(ref, 2));
    assert_isequal(size(obj, 3), size(ref, 3));
    assert_isequal(size(obj, [1 2]), size(ref, [1 2]));
    [r, c] = size(obj);
    [rr, cc] = size(ref);
    assert_isequal([r, c], [rr, cc]);
  end
end
%=============================================================================
% several dimensions can be queried at once
ref = zeros(2, 3, 4);
for m = 1:numel(makers)
  obj = makers{m}([2 3 4]);
  assert_isequal(size(obj, 1, 3), [2 4]);
  assert_isequal(size(obj, [1 3]), [2 4]);
  assert_isequal(size(obj, [3 1]), [4 2]);
  assert_isequal(size(obj, 1, 2, 3), [2 3 4]);
  assert_isequal(size(obj, 5, 1), [1 2]);
  assert_isequal(size(obj, [1 5 2]), [2 1 3]);
  [r, c] = size(obj, [1 3]);
  assert_isequal([r, c], [2 4]);
  [r, c] = size(obj, 1, 3);
  assert_isequal([r, c], [2 4]);
  assert_isequal(size(obj, 1, 3), size(ref, 1, 3));
  assert_isequal(size(obj, [1 3]), size(ref, [1 3]));
end
%=============================================================================
assert_isfalse(isscalar(seconds([1 2])));
assert_istrue(isscalar(seconds(1)));
assert_isfalse(isscalar(datetime(2020, 1, [1 2])));
assert_istrue(isscalar(datetime(2020, 1, 1)));
assert_isfalse(isscalar(calmonths([1 2])));
assert_isfalse(isscalar(seconds([])));
assert_isfalse(isscalar(NaT(1, 2)));
%=============================================================================
