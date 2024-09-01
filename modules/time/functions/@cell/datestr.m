%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = datestr(varargin)
  narginchk(1, 4);
  nargoutchk(0, 1);
  dateStringIn = varargin{1};
  if ~iscellstr(dateStringIn)
    error(_('First argument must be a cell array of row characters.'));
  end
  vars = varargin;
  vars{1} = string(dateStringIn);
  varargout{1} = datestr(vars{:});
end
%=============================================================================
