%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% mixed classes compare the values exactly
asserts.isfalse(isequal(int8(127), 200));
asserts.isfalse(isequal(int8(1), 1.4));
asserts.isfalse(isequal(int64(1), 1.5));
asserts.isfalse(isequal(single(0.1), 0.1));
asserts.istrue(isequal(single(0.5), 0.5));
asserts.istrue(isequal(int8(1), 1, true, char(1)));
asserts.istrue(isequal(sparse(1), int8(1)));
asserts.istrue(isequal(int8([1 2]), int16([1 2])));
%=============================================================================
% string arrays compare with character vectors and cell arrays of character vectors
asserts.istrue(isequal("abc", {'abc'}));
asserts.istrue(isequal(["a", "b"], {'a', 'b'}));
asserts.istrue(isequal(strings(0, 0), {}));
asserts.isfalse(isequal(strings(0, 0), []));
asserts.isfalse(isequal("1", 1));
%=============================================================================
% a missing string is never equal
asserts.isfalse(isequal(string(missing), string(missing)));
asserts.isfalse(isequal([missing, "a"], [missing, "a"]));
asserts.isfalse(isequal([missing, "a"], [missing, "b"]));
asserts.isfalse(isequal(["a", missing], ["a", "b"]));
asserts.isfalse(isequal({string(missing)}, {string(missing)}));
asserts.isfalse(isequal(missing, missing));
asserts.isfalse(isequal(missing, NaN));
%=============================================================================
% cells and structs are never equal to values of another class
asserts.isfalse(isequal({}, []));
asserts.isfalse(isequal({[]}, {{}}));
asserts.isfalse(isequal({[], {}}, {{}, []}));
asserts.isfalse(isequal(struct([]), []));
asserts.isfalse(isequal({}, struct('a', {})));
asserts.isfalse(isequal([], {}, ''));
%=============================================================================
% empty struct arrays: same size and same number of fields
asserts.istrue(isequal(struct('a', {}), struct('b', {})));
asserts.isfalse(isequal(struct([]), struct('a', {})));
asserts.isfalse(isequal(struct('a', {}), repmat(struct('a', 1), 1, 0)));
%=============================================================================
