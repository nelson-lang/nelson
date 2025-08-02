%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function file_generated = xmldoctohelp(varargin)
  % narginchk(3, 4);
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
  if ismodule('help_browser')
    helpbrowser('-close');
  end
  dir_output_tmp = [dir_output, 'nelson_help'];
  mkdir(dir_output_tmp);
  module_name = xmldocbuild(dirs_input, dir_output_tmp, main_title, 'help', overwrite);
  destination_file = '';
  if strcmp(module_name, '')
    destination_file = [nelsonappid(), '.modules.', 'manual', '.help', '.qch'];
  else
    destination_file = [nelsonappid(), '.modules.', module_name, '.help', '.qch'];
  end
  file_generated =  [dir_output, destination_file];
  
  qhelpgenerator(dir_output_tmp, dir_output, destination_file);
  file_generated =  [dir_output, destination_file];
  if ~isfile(file_generated)
    file_generated = '';
  end
end
%=============================================================================
