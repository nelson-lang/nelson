%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nf = nh5file(varargin)
  % Object that loads and saves parts of the variables of a .nh5 file.
  nf = nelson.io.Nh5File(varargin{:});
end
%=============================================================================
