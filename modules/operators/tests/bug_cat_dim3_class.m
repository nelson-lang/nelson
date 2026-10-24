%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% cat(dim, ...) with dim >= 3 lost the class of its operands
%=============================================================================
asserts.isequal(class(cat(3, int8(1), int8(2))), 'int8');
asserts.isequal(class(cat(3, single(1), single(2))), 'single');
asserts.isequal(class(cat(3, true, false)), 'logical');
asserts.isequal(class(cat(3, 'a', 'b')), 'char');
asserts.isequal(class(cat(3, uint16(1), uint16(2))), 'uint16');
asserts.isequal(class(cat(4, int8(1), int8(2))), 'int8');
%=============================================================================
R = cat(3, int8(1), int8(2));
asserts.isequal(R(:, :, 1), int8(1));
asserts.isequal(R(:, :, 2), int8(2));
%=============================================================================
C = cat(3, 'a', 'b');
asserts.isequal(C(:, :, 1), 'a');
asserts.isequal(C(:, :, 2), 'b');
%=============================================================================
L = cat(3, true, false);
asserts.isequal(L(:, :, 1), true);
asserts.isequal(L(:, :, 2), false);
%=============================================================================
% doubles still work
asserts.isequal(class(cat(3, 1, 2)), 'double');
asserts.isequal(size(cat(3, [1 2; 3 4], [5 6; 7 8])), [2 2 2]);
%=============================================================================
