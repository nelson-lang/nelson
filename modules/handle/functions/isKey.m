%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isKey(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  msg = sprintf(_("function not implemented for '%s' type."), class(varargin{1}));
  error(msg);
end
%=============================================================================
