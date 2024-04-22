%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = skip_testsuite(varargin)
  narginchk(0, 2);
  nargoutchk(0, 0);
  
  varargout = {};
  condition = false;
  reason = '';
  
  if nargin == 0
    condition = true;
  elseif nargin == 1
    if islogical(varargin{1})
      condition = varargin{1};
    else
      reason = varargin{1};
      condition = true;
    end
  else
    condition = varargin{1};
    reason = varargin{2};
  end
  
  if condition
    if isempty(reason)
      reason = _('Testsuite skipped');
    end
    error('Nelson:tests_manager:testsuite_skipped', reason);
  end
end
%=============================================================================