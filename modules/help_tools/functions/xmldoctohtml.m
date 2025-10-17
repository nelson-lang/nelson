%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xmldoctohtml(varargin)
  narginchk(3, 4);
  nargoutchk(0, 2);
  dirs_input = varargin{1};
  dir_output = varargin{2};
  main_title = varargin{3};
  html_type = 'web';
   if nargin() == 4
    overwrite = varargin{4};
  else
    overwrite = true;
  end
  [status, msg] = xmldocbuild(dirs_input, dir_output, main_title, html_type, overwrite);
  copyfile([modulepath('help_tools'), '/resources/highlight.pack.js'], dir_output);
  copyfile([modulepath('help_tools'), '/resources/highlight.css'], dir_output);
  copyfile([modulepath('help_tools'), '/resources/nelson_common.css'], dir_output);
  copyfile([modulepath('help_tools'), '/resources/nelson_help.js'], dir_output);

  varargout{1} = status;
  if nargout > 1
    varargout{2} = msg;
  end
end
%=============================================================================
