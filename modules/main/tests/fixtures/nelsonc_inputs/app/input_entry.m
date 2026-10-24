%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function input_entry()
  value = nested_value() + inputpkg.value();
  if value ~= 42
    error('Unexpected explicit function result.');
  end
  location = which('nested-data.txt');
  if isempty(location) || ~contains(fileread(location), 'NESTED_INPUT_OK')
    error('Nested data lookup failed.');
  end
  root = fileparts(fileparts(location));
  if ~isfolder(fullfile(root, 'empty'))
    error('Empty included directory is missing.');
  end
  if ~contains(fileread(which('data[1]+(a).txt')), 'PATTERN_INPUT_OK')
    error('Filename pattern resource is missing.');
  end
  if ~isempty(which('not-selected.txt'))
    error('Nonrecursive pattern included a nested resource.');
  end
  fprintf('INPUT_TREE_OK %d\n', value);
end
