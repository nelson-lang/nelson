%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% integer and single classes are preserved
R = kron(int8([1 2]), int8([3; 4]));
asserts.isequal(class(R), 'int8');
asserts.isequal(R, int8([3 6; 4 8]));
asserts.isequal(class(kron(single([1 2]), [3; 4])), 'single');
asserts.isequal(class(kron(uint16([1 2]), uint16([3 4]))), 'uint16');
asserts.isequal(kron(uint8([100 3]), uint8([4 5])), uint8([400 500 12 15]));
%=============================================================================
% double behavior unchanged
asserts.isequal(kron([1 2], [0 1]), [0 1 0 2]);
asserts.isequal(kron([1 3; 4 2], eye(2)), [1 0 3 0; 0 1 0 3; 4 0 2 0; 0 4 0 2]);
% complex
asserts.isequal(kron([1 + 2i 3], [2 1i]), [2 + 4i -2 + 1i 6 3i]);
%=============================================================================
