%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = runtime_inputnames_fixture(varargin)
  names = strings(1, nargin);
  for k = 1:nargin
    name = inputname(k);
    if isempty(name)
      names(k) = "";
    else
      names(k) = string(name);
    end
  end
end
%=============================================================================
