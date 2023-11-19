%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = substruct(varargin)
  narginchk(2, 1000);
  type = varargin(1:2:nargin);
  subs = varargin(2:2:nargin);
  nlevels = nargin / 2;
  if nlevels < 1
    error(_('At least two arguments.'));
  elseif rem(nlevels, 1)
    error(_('An even number of arguments.'));
  end
  s = struct('type', type, 'subs', subs);
end 
%=============================================================================
