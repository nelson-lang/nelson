%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function doc(varargin)
  if nargin() > 1
    error(_('Wrong number of input arguments.'));
  end
  if ~indexhelp()
    helpbrowser();
    helpbrowser('-hide', 'bookmarks');
  end
  if nargin() == 1
    name = varargin{1};
    if ~ischar(name)
      error(_('Wrong type for argument #1: string expected.'));
    end
    p = which(name,'-module');
    if isempty(p)
      helpbrowser('-name', name);
    else
      if length(p) == 1
        id = [p{1}, '::', name];
        helpbrowser('-identifier', id);
      else
        helpbrowser('-name', name);
      end
    end
  end
end
%==============================================================================
