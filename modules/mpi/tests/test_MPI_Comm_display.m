%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
%=============================================================================
comm = MPI_Comm_object();
%=============================================================================
R = evalc('display(comm)');
REF =  '
comm =

  1×1 handle [MPI_Comm] 

    Description:    MPI_COMM_WORLD

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('disp(comm)');
REF = '  1×1 handle [MPI_Comm] 

    Description:    MPI_COMM_WORLD
' ;
assert_isequal(R, REF)
%=============================================================================
delete(comm);
assert_isequal(length(MPI_Comm_used()), 0);
if MPI_Initialized()
  MPI_Finalize();
end
%=============================================================================
