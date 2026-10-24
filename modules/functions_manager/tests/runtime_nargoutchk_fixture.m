%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = runtime_nargoutchk_fixture()
  nargoutchk(0, 2);
  for k = 1:nargout
    varargout{k} = k;
  end
end
%=============================================================================
