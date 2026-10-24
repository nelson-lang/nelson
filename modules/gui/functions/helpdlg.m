%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = helpdlg(message, title)
  narginchk(0, 2);
  nargoutchk(0, 1);
  if nargin < 1
    message = _('This is the default help.');
  end
  if nargin < 2
    title = _('Help Dialog');
  end
  h = msgbox(message, title, 'help', 'non-modal');
  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
