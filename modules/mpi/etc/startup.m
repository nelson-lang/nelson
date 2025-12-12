%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
arch = computer('arch');
if strcmp(arch, 'woa64') == true
  % No MPI support on Windows on ARM64
  removemodule('mpi');
  return
end
if strcmp(getnelsonmode(), 'BASIC_TERMINAL') == true
  addgateway(modulepath('mpi', 'builtin'), 'mpi');
end
addpath(modulepath('mpi', 'functions'), '-frozen');
%=============================================================================
