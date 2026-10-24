%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Sparse groups must use their complete parent path inside structs and cells.
% Include nonempty zero matrices separately from matrices with empty dimensions.
fixtures = {sparse([1, 3, 2], [1, 2, 4], [3, -4, 7], 3, 4, 7), ...
  sparse([1, 3, 2], [1, 2, 4], [3 + 2i, -4i, 7 - 1i], 3, 4, 8), ...
  sparse([1, 3, 2], [1, 2, 4], [true, true, true], 3, 4, 9), ...
  sparse(zeros(3, 4)), logical(sparse(zeros(3, 4))), ...
  sparse(zeros(0, 4)), sparse(zeros(3, 0)), ...
  logical(sparse(zeros(0, 4))), logical(sparse(zeros(3, 0)))};
test_h5save_file = [tempname(), '.nh5'];
try
  for useCompression = [true, false]
    for fixtureIndex = 1:numel(fixtures)
      expected = fixtures{fixtureIndex};
      % The same field number occurs repeatedly in different parent groups.
      payload = struct('K', expected, 'nested', struct('copies', {{expected, expected}}));
      cells = {expected, struct('K', expected); {expected}, expected};
      topLevel = expected;
      if useCompression
        savenh5(test_h5save_file, 'payload', 'cells', 'topLevel');
      else
        savenh5(test_h5save_file, '-nocompression', 'payload', 'cells', 'topLevel');
      end
      for repetition = 1:3
        restored = loadnh5(test_h5save_file);
        asserts.isequal(sort(fieldnames(restored)), sort({'payload'; 'cells'; 'topLevel'}));
        asserts.isequal(restored.payload, payload);
        asserts.isequal(restored.cells, cells);
        checkSparse(restored.topLevel, expected);
        checkSparse(restored.payload.K, expected);
        checkSparse(restored.payload.nested.copies{1}, expected);
        checkSparse(restored.payload.nested.copies{2}, expected);
        checkSparse(restored.cells{1, 1}, expected);
        checkSparse(restored.cells{1, 2}.K, expected);
        nestedCell = restored.cells{2, 1};
        checkSparse(nestedCell{1}, expected);
        checkSparse(restored.cells{2, 2}, expected);
        % Inspect physical nested groups, not only the reconstructed values.
        marker = h5readatt(test_h5save_file, '/payload/0', 'NELSON_sparse');
        asserts.isequal(marker, uint8(1));
        marker = h5readatt(test_h5save_file, '/payload/1/0/1', 'NELSON_sparse');
        asserts.isequal(marker, uint8(1));
        marker = h5readatt(test_h5save_file, '/cells/0', 'NELSON_dimensions');
        asserts.isequal(marker, uint64(size(expected)'));
        % Overwrite the same nested paths while unrelated root variables survive.
        if useCompression
          savenh5(test_h5save_file, '-append', 'payload', 'cells');
        else
          savenh5(test_h5save_file, '-append', '-nocompression', 'payload', 'cells');
        end
      end
      payload = struct('K', sparse(1, 1, 17, 2, 2), 'nested', struct('copies', {{false}}));
      savenh5(test_h5save_file, '-append', 'payload');
      restored = loadnh5(test_h5save_file);
      checkSparse(restored.payload.K, payload.K);
      asserts.isequal(restored.payload.nested.copies, {false});
      asserts.isequal(restored.cells, cells);
      checkSparse(restored.topLevel, expected);
    end
  end
  rmfile(test_h5save_file);
catch ex
  if isfile(test_h5save_file)
    rmfile(test_h5save_file);
  end
  rethrow(ex);
end

function checkSparse(actual, expected)
  asserts.istrue(issparse(actual));
  asserts.isequal(size(actual), size(expected));
  asserts.isequal(class(actual), class(expected));
  asserts.isequal(isreal(actual), isreal(expected));
  asserts.isequal(full(actual), full(expected));
  if ~islogical(expected)
    asserts.isequal(full(real(actual)), full(real(expected)));
    asserts.isequal(full(imag(actual)), full(imag(expected)));
  end
end
