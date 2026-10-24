%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a missing string equals a missing string, like NaN
asserts.istrue(isequaln(string(missing), string(missing)));
asserts.istrue(isequaln([missing, "a"], [missing, "a"]));
asserts.isfalse(isequaln([missing, "a"], [missing, "b"]));
asserts.isfalse(isequaln(["a", missing], ["a", "b"]));
asserts.istrue(isequaln({string(missing)}, {string(missing)}));
asserts.isfalse(isequaln({string(missing)}, {''}));
asserts.istrue(isequaln(missing, missing));
asserts.istrue(isequaln({missing}, {missing}));
%=============================================================================
% nested elements of different class or size
asserts.istrue(isequaln({"a"}, {'a'}));
asserts.istrue(isequaln({1}, {int8(1)}));
asserts.isfalse(isequaln({[1 2]}, {[1; 2]}));
asserts.isfalse(isequaln({[1 2]}, {[1 2 3]}));
asserts.isfalse(isequaln(struct('a', [1 2]), struct('a', [1; 2])));
asserts.isfalse(isequaln(struct('a', [1 2]), struct('a', [1 2 3])));
asserts.isfalse(isequaln({1, 2}, {1; 2}));
%=============================================================================
% mixed classes compare the values exactly
asserts.isfalse(isequaln(NaN, int8(0)));
asserts.isfalse(isequaln(int8(1), 1.4));
asserts.istrue(isequaln(sparse(NaN), NaN));
%=============================================================================
% empty struct arrays: same size and same number of fields
asserts.istrue(isequaln(struct('a', {}), struct('b', {})));
asserts.isfalse(isequaln(struct([]), struct('a', {})));
%=============================================================================
