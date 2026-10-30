%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% MPI worker: each rank computes a horizontal band of the Mandelbrot set.
if ~MPI_Initialized()
  MPI_Init();
end
workerRank = MPI_Comm_rank();
workerRanks = MPI_Comm_size();
workerTag = 7;

workerWidth = 360;
workerHeight = 360;
workerMaxIterations = 60;
workerRealMin = -2.2;
workerRealMax = 0.8;
workerImagMin = -1.5;
workerImagMax = 1.5;
workerRowsPerRank = ceil(workerHeight / workerRanks);
workerFirstRow = workerRank * workerRowsPerRank + 1;
workerLastRow = min((workerRank + 1) * workerRowsPerRank, workerHeight);

workerBand = [];
if workerFirstRow <= workerHeight
  workerBand = mandelbrotWorkerBand(workerFirstRow, workerLastRow, ...
    workerWidth, workerHeight, workerRealMin, workerRealMax, ...
    workerImagMin, workerImagMax, workerMaxIterations);
end

if workerRank == 0
  escapeImage = zeros(workerHeight, workerWidth);
  rankOwner = zeros(workerHeight, 1);
  if ~isempty(workerBand)
    escapeImage(workerFirstRow:workerLastRow, :) = workerBand;
    rankOwner(workerFirstRow:workerLastRow) = 0;
  end
  for sourceRank = 1:workerRanks - 1
    sourceFirst = sourceRank * workerRowsPerRank + 1;
    sourceLast = min((sourceRank + 1) * workerRowsPerRank, workerHeight);
    if sourceFirst <= workerHeight
      received = MPI_Recv(sourceRank, workerTag);
      escapeImage(sourceFirst:sourceLast, :) = received;
      rankOwner(sourceFirst:sourceLast) = sourceRank;
    end
  end
  mpiResultRanks = workerRanks;
  mpiResultMaxIterations = workerMaxIterations;
  outputFile = [tempdir(), 'nelson_mpi_mandelbrot.mat'];
  savemat(outputFile, 'escapeImage', 'rankOwner', 'mpiResultRanks', ...
    'mpiResultMaxIterations');
else
  if ~isempty(workerBand)
    MPI_Send(workerBand, 0, workerTag);
  end
end

if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
function band = mandelbrotWorkerBand(firstRow, lastRow, width, height, ...
  realMin, realMax, imagMin, imagMax, maxIterations)
  realAxis = linspace(realMin, realMax, width);
  imagAxis = linspace(imagMin, imagMax, height);
  [realGrid, imagGrid] = meshgrid(realAxis, imagAxis(firstRow:lastRow));
  c = complex(realGrid, imagGrid);
  z = c;
  band = zeros(lastRow - firstRow + 1, width);
  for iteration = 1:maxIterations
    z = z .* z + c;
    band = band + (abs(z) <= 2);
  end
end
%=============================================================================
