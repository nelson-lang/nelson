%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[v1, v2] = MPI_Get_version();
assert_istrue(isscalar(v1));
assert_istrue(isscalar(v2));
assert_istrue(isdouble(v1));
assert_istrue(isdouble(v1));
assert_istrue(v1 > 0);
%=============================================================================
