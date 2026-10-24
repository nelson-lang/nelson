%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempname(), '.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
d = dictionary([1 2], ["one" "two"]);
configuredEmpty = configureDictionary("string", "double");
unconfigured = dictionary();
cellValues = dictionary(string({'a', 'b', 'nested'}), {1, [2 3], struct('payload', {10, 20})});
savenh5(test_h5save_file, 'd', 'configuredEmpty', 'unconfigured', 'cellValues');
%=============================================================================
R = h5readatt(test_h5save_file, '/d', 'NELSON_class');
asserts.isequal(R, 'dictionary');
R = h5readatt(test_h5save_file, '/d', 'NELSON_object');
asserts.isequal(R, uint8(1));
%=============================================================================
clear d configuredEmpty unconfigured cellValues;
loadnh5(test_h5save_file);
%=============================================================================
asserts.isequal(class(d), 'dictionary');
asserts.isequal(keys(d), [1; 2]);
asserts.isequal(values(d), ["one"; "two"]);
asserts.isequal(lookup(d, 1), "one");
d.map = struct();
asserts.isequal(lookup(d, 2), "two");
[keyType, valueType] = types(d);
asserts.isequal(keyType, "double");
asserts.isequal(valueType, "string");
%=============================================================================
asserts.isequal(class(configuredEmpty), 'dictionary');
asserts.istrue(isConfigured(configuredEmpty));
asserts.isequal(numEntries(configuredEmpty), 0);
[keyType, valueType] = types(configuredEmpty);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "double");
%=============================================================================
asserts.isequal(class(unconfigured), 'dictionary');
asserts.isfalse(isConfigured(unconfigured));
%=============================================================================
asserts.isequal(class(cellValues), 'dictionary');
asserts.isequal(cellValues{"b"}, [2 3]);
asserts.isequal(cellValues{"nested"}(2).payload, 20);
%=============================================================================
rmfile(test_h5save_file);
%=============================================================================
