%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'a', ''}, {'a', 'b'}, {'a', 'b'});
asserts.isequal(isundefined(A), [false true]);
asserts.isequal(isundefined(ones(2, 3)), false(2, 3));
%=============================================================================
