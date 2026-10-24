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
  % xmldoctomd(source_dirs, destination_dir, main_title [, overwrite [, index_dirs]])
  % index_dirs: XML help roots of the other modules, used to resolve
  % <link linkend="${module}name"> to the real page path.
  if nargin() < 3
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nargin() > 5
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
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
  mkdir(dir_output);
  xmldocbuild(dirs_input, dir_output, main_title, 'md', overwrite, index_dirs);
  tf = true;
end
%=============================================================================
