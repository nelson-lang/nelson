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
asserts.istrue(isscalar(v1));
asserts.istrue(isscalar(v2));
asserts.istrue(isdouble(v1));
asserts.istrue(isdouble(v1));
asserts.istrue(v1 > 0);
%=============================================================================
