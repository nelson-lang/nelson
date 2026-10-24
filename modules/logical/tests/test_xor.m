%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('xor'), 2);
asserts.isequal(nargout('xor'), 1);
%=============================================================================
asserts.isequal(xor([true false true], [false false true]), [true false false]);
asserts.isequal(xor(1, 0), true);
asserts.isequal(xor(1, 1), false);
%=============================================================================
