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
% https://github.com/nelson-lang/nelson/issues/104
% <-- Short Description -->
% mpiexec did not work on some linux
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
mpiexec([modulepath('mpi'), '/examples/MPI_helloworld.m'], 4)
