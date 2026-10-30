%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Skip when the MPI launcher cannot start local workers in this environment.
mpiWorksHere = false;
try
  [launchStatus, ~] = mpiexec([modulepath('mpi', 'root'), ...
    '/examples/MPI_helloworld.m'], 2);
  mpiWorksHere = (launchStatus == 0);
catch
end
skip_testsuite(~mpiWorksHere, 'MPI workers could not be launched here.');
%=============================================================================
pmFigureVisible = 'off';
pmAnimationEnabled = true;
pmAnimationFrames = 4;
pmFramePause = 0;
pmAnimationContinuous = false;
pmProcessCount = 4;
run([modulepath('mpi', 'root'), '/examples/parallel_mandelbrot.m']);

assert_istrue(isgraphics(pmFigure));
assert_istrue(isgraphics(pmImage));
% The result came back from four MPI ranks through mpiexec.
assert_isequal(pmStatus, 0);
assert_isequal(pmRanks, 4);
assert_isequal(size(pmEscape), [pmHeight pmWidth]);
% Every row was assigned to one of the four ranks.
assert_isequal(numel(unique(pmRankOwner)), 4);
delete(pmFigure);
drawnow();
%=============================================================================
