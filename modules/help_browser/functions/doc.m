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
  narginchk(0, 1);
  if ~indexhelp()
    helpbrowser();
  end
  if nargin() == 0
    helpbrowser('-show');
    return
  end
  name = varargin{1};
  mustBeTextScalar(name, 1);
  if ismacro(name) || isbuiltin(name)
    helpbrowser('-name', name);
    helpbrowser('-show');
    return
  end
  if ismodule(name)
    helpbrowser('-module', name);
    helpbrowser('-show');
    return
  end
  p = which(name,'-module');
  if isempty(p)
    helpbrowser('-name', name);
    helpbrowser('-show');
    return
  end
  if length(p) == 1
    id = [p{1}, '::', char(name)];
    helpbrowser('-identifier', id);
  else
    helpbrowser('-name', name);
  end
  helpbrowser('-show');
end
%==============================================================================
