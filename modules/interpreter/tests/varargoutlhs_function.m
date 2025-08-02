%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = varargoutlhs_function(count)
  disp(nargout);
  switch count
    case 0
      disp('ok');
      varargout = {};
    case 1
      varargout{1} = 1;
    otherwise
      varargout{1} = 1;
      varargout{2} = 2;
    end
  end
  %=============================================================================
