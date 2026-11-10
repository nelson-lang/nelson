%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mf = matfile(varargin)
  % Object that loads and saves parts of the variables of a MAT-file.
  mf = nelson.io.MatFile(varargin{:});
end
%=============================================================================
