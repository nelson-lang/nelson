%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v7', '-v7.3'};
%=============================================================================
for v = matver
  test_file_mat = [tempname(), '.mat'];
  d = dictionary([1 2], [3 4]);
  dStringValues = dictionary([1 2], string({'one', 'two'}));
  configuredEmpty = configureDictionary("string", "double");
  unconfigured = dictionary();
  cellValues = dictionary(string({'a', 'b', 'nested'}), {1, [2 3], struct('payload', {10, 20})});
  savemat(test_file_mat, 'd', 'dStringValues', 'configuredEmpty', 'unconfigured', 'cellValues', v{1});
  clear d dStringValues configuredEmpty unconfigured cellValues;
  loadmat(test_file_mat);

  asserts.isequal(class(d), 'dictionary');
  asserts.isequal(keys(d), [1; 2]);
  asserts.isequal(values(d), [3; 4]);
  asserts.isequal(lookup(d, 1), 3);
  d.map = struct();
  asserts.isequal(lookup(d, 2), 4);
  [keyType, valueType] = types(d);
  asserts.isequal(keyType, "double");
  asserts.isequal(valueType, "double");

  asserts.isequal(class(dStringValues), 'dictionary');
  asserts.isequal(values(dStringValues), ["one"; "two"]);
  asserts.isequal(lookup(dStringValues, 1), "one");

  asserts.isequal(class(configuredEmpty), 'dictionary');
  asserts.istrue(isConfigured(configuredEmpty));
  asserts.isequal(numEntries(configuredEmpty), 0);
  [keyType, valueType] = types(configuredEmpty);
  asserts.isequal(keyType, "string");
  asserts.isequal(valueType, "double");

  asserts.isequal(class(unconfigured), 'dictionary');
  asserts.isfalse(isConfigured(unconfigured));

  asserts.isequal(class(cellValues), 'dictionary');
  asserts.isequal(cellValues{"b"}, [2 3]);
  asserts.isequal(cellValues{"nested"}(2).payload, 20);
  rmfile(test_file_mat);
end
%=============================================================================
