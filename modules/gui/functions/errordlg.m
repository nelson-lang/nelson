%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = errordlg(message, title, mode)
  narginchk(0, 3);
  nargoutchk(0, 1);
  if nargin < 1
    message = 'This is the default error.';
  end
  if nargin < 2
    title = _('Error Dialog');
  end
  if nargin < 3
    mode = 'non-modal';
  end
  h = msgbox(message, title, 'error', mode);
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
