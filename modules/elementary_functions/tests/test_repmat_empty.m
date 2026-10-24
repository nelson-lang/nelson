%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A zero dimension makes the result empty even when another dimension is nonzero.
% Preserve every remaining dimension and the source class on both copy paths.
classes = {'double', 'single', 'int8', 'uint8', 'int16', 'uint16', ...
  'int32', 'uint32', 'int64', 'uint64', 'logical', 'char'};
shapes = {[0, 1], [0, 3], [3, 0], [0, 0]};
for k = 1:numel(classes)
  for j = 1:numel(shapes)
    dimensions = shapes{j};
    source = cast(zeros(dimensions), classes{k});
    result = repmat(source, 2, 4);
    asserts.isequal(size(result), dimensions .* [2, 4]);
    asserts.isequal(class(result), classes{k});
    asserts.istrue(isempty(result));
    asserts.isequal(repmat(source, [2, 4]), result);
    asserts.isequal(size(repmat(source, 3)), dimensions * 3);
    asserts.isequal(size(repmat(source, 1, 1)), dimensions);
  end
end
% Complex data, both zero strides and a later zero dimension in the ND path.
for precision = {'double', 'single'}
  for shape = {[0, 2, 3], [2, 0, 3], [2, 3, 0]}
    source = complex(cast(zeros(shape{1}), precision{1}));
    result = repmat(source, [2, 3, 4]);
    asserts.isequal(size(result), shape{1} .* [2, 3, 4]);
    asserts.isequal(class(result), precision{1});
    asserts.istrue(isempty(result));
  end
end
% Empty copy paths still preserve fields and managed element types.
sources = {cell(0, 3), strings(0, 3), struct('tag', cell(0, 3)), ...
  repmat(@sin, 0, 3)};
for k = 1:numel(sources)
  source = sources{k};
  result = repmat(source, 2, 4);
  asserts.isequal(size(result), [0, 12]);
  asserts.isequal(class(result), class(source));
  asserts.istrue(isempty(result));
  expanded = repmat(source, [2, 4, 3]);
  asserts.isequal(size(expanded), [0, 12, 3]);
  asserts.isequal(class(expanded), class(source));
  if isstruct(source)
    asserts.isequal(fieldnames(result), {'tag'});
    asserts.isequal(fieldnames(expanded), {'tag'});
  end
end
% An empty source and a zero replication factor are independent cases.
asserts.isequal(size(repmat(zeros(0, 3), 2, 0)), [0, 0]);
asserts.isequal(size(repmat(zeros(3, 0), [0, 2, 4])), [0, 0, 4]);
asserts.isequal(size(repmat(reshape(1:6, [2, 3]), [2, 0, 4])), [4, 0, 4]);
for shape = {[0, 3], [3, 0], [0, 0]}
  source = sparse(zeros(shape{1}));
  result = repmat(source, 2, 4);
  asserts.isequal(size(result), shape{1} .* [2, 4]);
  asserts.istrue(issparse(result));
  asserts.isequal(nnz(result), 0);
end
