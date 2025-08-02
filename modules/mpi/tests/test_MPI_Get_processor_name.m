%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--MPI MODE-->
%=============================================================================
assert_checkerror('name = MPI_Get_processor_name()', _('Attempting to use an MPI routine before initializing MPI.'));
MPI_Init();
[name, len, info] = MPI_Get_processor_name();
MPI_Finalize();
assert_istrue(ischar(name));
assert_isequal(length(name), len);
assert_isequal(info, 0);
%=============================================================================
