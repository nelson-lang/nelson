%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = have_mpi()
  persistent mpi_required %#ok<NLS0005>
  if isempty(mpi_required)
    nelson_cli = getNelsonExecutablePath('nelson-cli');
    command = ['mpiexec -n 1 "', nelson_cli, '" -q -e "exit()"'];
    mpi_required = (unix(command) == 0);
  end
  r = mpi_required;
end
