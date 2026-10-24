%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __delete_files__(varargin)
  % Shared implementation of delete for filenames given as character
  % vectors, string arrays or cell arrays of character vectors.
  % Filenames can include wildcards (*).
  for k = 1:nargin
    value = varargin{k};
    if ischar(value)
      deleteOnePattern(value);
    elseif isstring(value) || iscell(value)
      for j = 1:numel(value)
        if iscell(value)
          deleteOnePattern(value{j});
        else
          deleteOnePattern(value(j));
        end
      end
    else
      error('Nelson:files_folders_functions:validFilenameExpected', 'A valid filename expected.');
    end
  end
end
%=============================================================================
function deleteOnePattern(pattern)
  if isstring(pattern) && isscalar(pattern) && ~ismissing(pattern)
    pattern = char(pattern);
  end
  if ~ischar(pattern) || (~isempty(pattern) && size(pattern, 1) ~= 1)
    error('Nelson:files_folders_functions:validFilenameExpected', 'A valid filename expected.');
  end
  entries = dir(pattern);
  if ~isempty(entries)
    entries = entries(~[entries.isdir]);
  end
  if isempty(entries)
    warning('Nelson:FileNotFound', 'File ''%s'' not found.', pattern);
    return
  end
  for j = 1:numel(entries)
    [status, msg] = rmfile([entries(j).folder, '/', entries(j).name]);
    if ~status
      warning('Nelson:DELETE:Permission', msg);
    end
  end
end
%=============================================================================
