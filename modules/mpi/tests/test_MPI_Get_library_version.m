%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
libname = MPI_Get_library_version();
asserts.istrue(ischar(libname));
asserts.istrue(length(libname) > 0);
%=============================================================================
