%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nan([]), zeros([0 0], 'double'));
asserts.isequal(nan(true), NaN);
asserts.isequal(nan(2,'LIKE',single(1)), single(reshape([NaN NaN NaN NaN], [2 2])));
asserts.isequal(nan(1,[]), zeros([1 0], 'double'));
asserts.isequal(nan(2,3,[]), zeros([2 3 0], 'double'));
asserts.isequal(nan("single"), single(NaN));
asserts.isequal(nan([2 3],"single"), single(reshape([NaN NaN NaN NaN NaN NaN], [2 3])));
%=============================================================================
