%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = nmm_is_installed(module_name, varargin)
  if nargin == 2
    [~, module_data] = nmm_find_installed_module(module_name, varargin{1});
  elseif nargin == 1
    [~, module_data] = nmm_find_installed_module(module_name);
  else
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  r = ~isempty(module_data);
end
%=============================================================================
