%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% (5a) complex value with a zero imaginary part shows only the real part.
asserts.isequal(mat2str(complex(1, 0)), '1');
asserts.isequal(mat2str([complex(1, 0) complex(2, 3)]), '[1 2+3i]');
%=============================================================================
% (5b) character arrays and string arrays are supported.
asserts.isequal(mat2str('FF'), '''FF''');
asserts.isequal(mat2str(['ab'; 'cd']), '[''ab'';''cd'']');
asserts.isequal(mat2str("ab"), '"ab"');
asserts.isequal(mat2str(["ab" "cd"]), '["ab" "cd"]');
asserts.isequal(mat2str(["ab"; "cd"]), '["ab";"cd"]');
%=============================================================================
% options in any order, sparse rows and empty sparse matrices, text arrays.
asserts.isequal(mat2str([1 2], 3, 'class'), 'double([1 2])');
asserts.isequal(mat2str([1 2], 'class', 3), 'double([1 2])');
asserts.isequal(mat2str(pi, 'CLASS'), 'double(3.14159265358979)');
asserts.isequal(mat2str(sparse([1 0 2])), 'sparse([1 1], [1 3], [1 2], 1, 3)');
asserts.isequal(mat2str(sparse([1 0 2]), 'class'), 'sparse([1 1], [1 3], double([1 2]), 1, 3)');
asserts.isequal(mat2str(sparse([1; 0; 2])), 'sparse([1;3], [1;1], [1;2], 3, 1)');
asserts.isequal(mat2str(sparse(2, 2)), 'sparse(zeros(0,1), zeros(0,1), zeros(0,1), 2, 2)');
asserts.isequal(mat2str(sparse(1, 3)), 'sparse(zeros(1,0), zeros(1,0), zeros(1,0), 1, 3)');
asserts.isequal(mat2str(sparse([true false true])), 'sparse([1 1], [1 3], [true true], 1, 3)');
asserts.isequal(mat2str('abc', 'class'), '''abc''');
asserts.isequal(mat2str("abc", 'class'), '"abc"');
asserts.isequal(mat2str(strings(0, 1)), 'strings(0,1)');
asserts.isequal(mat2str(string(missing)), 'string(missing)');
%=============================================================================
