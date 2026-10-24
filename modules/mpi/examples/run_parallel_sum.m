%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Launch the parallel sum example with four local workers.
if ispc()
  probeCommand = 'mpiexec /?';
else
  probeCommand = 'mpiexec --version';
end
[probeStatus, probeOutput] = system(probeCommand);
if probeStatus ~= 0
  error(['This example requires an MPI launcher available on PATH: ', probeOutput]);
end
script = [modulepath('mpi'), '/examples/MPI_parallel_sum.m'];
[status, output] = mpiexec(script, 4);
if status ~= 0
  error(['The MPI example failed: ', output]);
end
disp(output);
