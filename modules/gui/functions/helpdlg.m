%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = helpdlg(varargin)
  narginchk(0, 3);
  nargoutchk(0, 1);
  if nargin < 1
    helpstring = _('This is the default help string.');
  else
    helpstring = varargin{1};
  end
  if nargin < 2
    dlgname = _('Help Dialog');
  else
    dlgname = varargin{2};
  end
  if nargin < 3
    mode = 'non-modal';
  else
    mode = varargin{3};
  end
  h = msgbox(helpstring, dlgname, 'help', mode);
end
%=============================================================================
