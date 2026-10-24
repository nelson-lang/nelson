%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function common_entry()
  name = fullfile(fileparts(mfilename('fullpath')), '..', 'data', 'sibling.txt');
  if ~contains(fileread(name), 'SIBLING_INPUT_OK')
    error('Selected ancestor did not preserve sibling paths.');
  end
  fprintf('INPUT_TREE_OK 42\n');
end
