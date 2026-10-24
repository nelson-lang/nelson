%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
asserts.isequal(bytecode_simple_macro_leaf(4), 13);
asserts.isequal(bytecode_simple_macro_leaf(0.5), 2.5);
asserts.isequal(bytecode_simple_macro_leaf([1 2 3]), [4 7 10]);
asserts.isequal(bytecode_simple_macro_identity(9), 9);
asserts.isequal(bytecode_simple_macro_scale(9), 27);

eval('bytecode_simple_macro_leaf = [5 6 7]; asserts.isequal(bytecode_simple_macro_leaf(2), 6); clear bytecode_simple_macro_leaf');

asserts.isequal(bytecode_simple_macro_leaf(5), 16);

value = 0;
for k = 1:10
  value = value + bytecode_simple_macro_leaf(k);
end
asserts.isequal(value, 175);
