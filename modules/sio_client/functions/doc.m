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
  % replaces standard 'doc' function to open help in web browser
  url_help = 'https://nelson-9.gitbook.io/nelson/en/';
  if nargin() > 1
    error(_('Wrong number of input arguments.'));
  end
  if nargin() == 1
    name = varargin{1};
    if ~ischar(name)
      error(_('Wrong type for argument #1: string expected.'));
    end
    module = which(name,'-module');
    if isempty(module)
      sioemit('help', [url_help , '?q=', name]);
    else
      if length(module) == 1
        sioemit('help', [url_help, module{1}, '/', name, '.html']);
      else
        sioemit('help', [url_help , '?q=', name]);
      end
    end
  else
   sioemit('help', url_help);
  end
end
%=============================================================================
