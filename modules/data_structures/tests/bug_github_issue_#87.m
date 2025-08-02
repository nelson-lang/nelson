%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/87
% <-- Short Description -->
% struct did not support sparse matrix.
%=============================================================================
st = struct('a', sparse(3, 3));
ref.a = sparse(3, 3);
assert_isequal(st, ref);
%=============================================================================
