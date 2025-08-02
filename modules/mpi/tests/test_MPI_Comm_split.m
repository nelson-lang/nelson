%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--MPI MODE-->
%=============================================================================
if ~MPI_Initialized()
  MPI_Init();
end
comm = MPI_Comm_object();
world_rank = MPI_Comm_rank();
world_size = MPI_Comm_size();

color = world_rank * inv(4);

% Split the communicator based on the color and use the
% original rank for ordering
row_comm = MPI_Comm_split(comm, color, world_rank);

row_rank = MPI_Comm_rank();
row_size = MPI_Comm_size();

disp(['WORLD RANK/SIZE: ',int2str(world_rank), '/', int2str(world_size), ' ROW RANK/SIZE: ', int2str(row_rank), '/', int2str(row_size)]);
if MPI_Initialized()
  MPI_Finalize();
end

%=============================================================================
