%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = xmldoctomd(varargin)
  if nargin() < 3 || nargin() > 4
    error(_('Wrong number of input arguments.'));
  end
  dirs_input = varargin{1};
  dir_output = varargin{2};
  main_title = varargin{3};
  if nargin() == 4
    overwrite = varargin{4};
  else
    overwrite = true;
  end
  mkdir(dir_output);
  module_name = xmldocbuild(dirs_input, dir_output, main_title, 'md', overwrite);
  tf = true;
end
%=============================================================================
