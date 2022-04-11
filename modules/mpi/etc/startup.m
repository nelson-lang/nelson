%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
with_dl_mpi = true;
if ispc()
  try
    dlh = dlopen('msmpi.dll');
    dlclose(dlh);
    with_dl_mpi = true;
  catch
    with_dl_mpi = false;
  end
end
if with_dl_mpi == true
  addgateway(modulepath(nelsonroot(), 'mpi', 'builtin'));
  addpath(modulepath(nelsonroot(), 'mpi', 'functions'), '-frozen');
else
  fprintf(stderr, _('MPI dependency not loaded.'));
  removemodule('mpi');
end
%=============================================================================
