%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xmldoctotypst(varargin)
  % xmldoctotypst converts xml Nelson help files to Typst sources.
  % [status, msg] = xmldoctotypst(source_dirs, destination_dir, main_title [, overwrite [, index_dirs]])
  % index_dirs: XML help roots of the other modules, used to resolve
  % <link linkend="${module}name"> to the real page path.
  narginchk(3, 5);
  nargoutchk(0, 2);
  dirs_input = varargin{1};
  dir_output = varargin{2};
  main_title = varargin{3};
  if nargin() >= 4
    overwrite = varargin{4};
  else
    overwrite = true;
  end
  if nargin() == 5
    index_dirs = varargin{5};
  else
    index_dirs = {};
  end
  if ~isdir(dir_output)
    mkdir(dir_output);
  end
  [status, msg] = xmldocbuild(dirs_input, dir_output, main_title, 'typ', overwrite, index_dirs);
  if status
    % generated pages import this file from the module root
    copyfile([modulepath('help_tools'), '/resources/nelson_help.typ'], dir_output);
  end
  varargout{1} = status;
  if nargout > 1
    varargout{2} = msg;
  end
end
%=============================================================================
