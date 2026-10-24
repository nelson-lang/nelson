%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nflowPath = NFlow.internal.sspImport(sspFile[, destinationDirectory])
%
% Internal RPC helper invoked by the editor's Open-SSP action from the
% interpreter thread. Delegates to the public NFlow.sspImport; kept as a package
% function so the C++ bridge can resolve it by name (a classdef static method is
% not resolvable through the bridge's name lookup, a package function is).
%=============================================================================
function nflowPath = sspImport(sspFile, destinationDirectory)
  narginchk(1, 2);
  if nargin < 2
    nflowPath = NFlow.sspImport(sspFile);
  else
    nflowPath = NFlow.sspImport(sspFile, destinationDirectory);
  end
end
%=============================================================================
