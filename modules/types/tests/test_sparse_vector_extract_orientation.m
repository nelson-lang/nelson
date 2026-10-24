%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sparse storage and visible dimensions must agree after vector indexing.
x = sparse([.03; .03]);
y = x([1, 2]);
asserts.isequal(size(y), [2, 1]);
asserts.isequal(full(y), [.03; .03]);
asserts.isequal(full(y(2)), .03);
asserts.isapprox(y, [.03; .03], 0, 1e-12);
asserts.isequal(full(y - [.03; .03]), zeros(2, 1));
asserts.isequal(full([.03; .03] - y), zeros(2, 1));
asserts.isequal(full(y - sparse([.03; .03])), zeros(2, 1));
%=============================================================================
% Vector sources retain their orientation for either orientation of indices.
% Repetition, permutation and structural zeros exercise the stored coordinates.
values = {[2, 0, -3, 7], [2 + 3i, 0, -3 + 4i, 7 - 2i], logical([1, 0, 0, 1])};
indexSets = {[1, 2, 3, 4], [1; 2; 3; 4], [4, 1, 4, 2], [4; 1; 4; 2]};
for kind = 1:numel(values)
  for orientation = 1:2
    dense = values{kind};
    if orientation == 2
      dense = dense.';
    end
    source = sparse(dense);
    for item = 1:numel(indexSets)
      selected = indexSets{item};
      actual = source(selected);
      expected = dense(selected);
      asserts.istrue(issparse(actual));
      asserts.isequal(size(actual), size(expected));
      asserts.isequal(full(actual), expected);
      for k = 1:numel(expected)
        asserts.isequal(full(actual(k)), expected(k));
      end
      asserts.isequal(full(actual([4, 2, 1])), expected([4, 2, 1]));
      if ~islogical(expected)
        assert_zero(actual - expected);
        assert_zero(expected - actual);
        assert_zero(actual - sparse(expected));
      end
    end
    % A matrix-shaped index keeps its own shape even for a vector source.
    actual = source([4, 1; 2, 3]);
    expected = dense([4, 1; 2, 3]);
    asserts.isequal(size(actual), [2, 2]);
    asserts.isequal(full(actual), expected);
    asserts.isequal(full(actual(2, 1)), expected(2, 1));
    % Logical indexing also preserves the source vector orientation.
    for mask = {logical([1, 0, 1, 1]), logical([1; 0; 1; 1])}
      actual = source(mask{1});
      expected = dense(mask{1});
      asserts.isequal(size(actual), size(expected));
      asserts.isequal(full(actual), expected);
      asserts.isequal(full(actual(3)), expected(3));
    end
  end
end
%=============================================================================
% Matrix sources follow the numeric index shape; nested fields share the path.
dense = [2, 0, 7; -1, 4, 0];
source = sparse(dense);
for selected = {[6, 1, 3, 2], [6; 1; 3; 2], [6, 3; 1, 2]}
  actual = source(selected{1});
  expected = dense(selected{1});
  asserts.isequal(size(actual), size(expected));
  asserts.isequal(full(actual), expected);
  for k = 1:numel(expected)
    asserts.isequal(full(actual(k)), expected(k));
  end
end
holder = struct('inner', struct('values', sparse([0; 2; 0; 7])));
actual = holder.inner.values([4, 2, 4]);
asserts.isequal(size(actual), [3, 1]);
asserts.isequal(full(actual(2)), 2);
asserts.isequal(full(actual - [7; 2; 7]), zeros(3, 1));

function assert_zero(value)
  % Check both complex components without assuming storage decomplexification.
  value = full(value);
  asserts.isequal(real(value), zeros(size(value)));
  asserts.isequal(imag(value), zeros(size(value)));
end
