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
asserts.checkerror('name = MPI_Get_processor_name()', _('MPI must be initialized.'));
MPI_Init();
[name, len, info] = MPI_Get_processor_name();
MPI_Finalize();
asserts.istrue(ischar(name));
asserts.isequal(length(name), len);
asserts.isequal(info, 0);
%=============================================================================
