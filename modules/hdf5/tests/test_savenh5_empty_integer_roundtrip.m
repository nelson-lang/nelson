%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Empty integer reads must report success, including zero-nnz sparse indices.
integerClasses = {'int8', 'uint8', 'int16', 'uint16', 'int32', 'uint32', 'int64', 'uint64'};
shapes = {[0, 0], [1, 0], [0, 3], [2, 0, 4]};
test_h5save_file = [tempname(), '.nh5'];
try
  for useCompression = [true, false]
    for classIndex = 1:numel(integerClasses)
      for shapeIndex = 1:numel(shapes)
        value = feval(integerClasses{classIndex}, zeros(shapes{shapeIndex}));
        payload = struct('empty', value, 'tag', int32(71));
        cells = {uint8(1), value; struct('empty', value), {value}};
        if useCompression
          savenh5(test_h5save_file, 'value', 'payload', 'cells');
        else
          savenh5(test_h5save_file, '-nocompression', 'value', 'payload', 'cells');
        end
        restored = loadnh5(test_h5save_file);
        checkEmpty(restored.value, value);
        checkEmpty(restored.payload.empty, value);
        checkEmpty(restored.cells{1, 2}, value);
        checkEmpty(restored.cells{2, 1}.empty, value);
        nestedCell = restored.cells{2, 2};
        checkEmpty(nestedCell{1}, value);
        asserts.isequal(restored.payload, payload);
        asserts.isequal(restored.cells, cells);
        asserts.isequal(sort(fieldnames(restored)), sort({'value'; 'payload'; 'cells'}));
        storedClass = h5readatt(test_h5save_file, '/value', 'NELSON_class');
        asserts.isequal(storedClass, integerClasses{classIndex});
        storedDimensions = h5readatt(test_h5save_file, '/value', 'NELSON_dimensions');
        asserts.isequal(storedDimensions, uint64(size(value)'));
        asserts.isequal(h5readatt(test_h5save_file, '/value', 'NELSON_empty'), uint8(1));
      end
    end
  end
  rmfile(test_h5save_file);
catch ex
  if isfile(test_h5save_file)
    rmfile(test_h5save_file);
  end
  rethrow(ex);
end

function checkEmpty(actual, expected)
  asserts.istrue(isempty(actual));
  asserts.isequal(class(actual), class(expected));
  asserts.isequal(size(actual), size(expected));
  asserts.isequal(actual, expected);
end
