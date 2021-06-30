%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    destination_file = ['org.nelson.modules.', 'manual', '.help', '.qch'];
  else
    destination_file = ['org.nelson.modules.', module_name, '.help', '.qch'];
  end
  file_generated =  [dir_output, destination_file];

  qhelpgenerator(dir_output_tmp, dir_output, destination_file);
  file_generated =  [dir_output, destination_file];
  if ~isfile(file_generated)
    file_generated = '';
  end
end
%=============================================================================