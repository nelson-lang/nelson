%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~strcmp(getenv('NELSON_WITHOUT_MPI'), 'TRUE')
  try
    addmodule([nelsonroot() '/modules/' 'mpi'], 'mpi');
  catch
    warning('MPI not loaded');
  end
end
%=============================================================================
