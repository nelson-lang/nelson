%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  tf = varargin{1};
  name = varargin{2};
  if ~isprop(tf, name)
    msg = _('No property of the class ''%s'' matches the identifier ''%s''.');
    error(sprintf(msg, 'tf', name));
  end
  st = struct(tf);
  varargout{1} = st.(name);
end
%=============================================================================
