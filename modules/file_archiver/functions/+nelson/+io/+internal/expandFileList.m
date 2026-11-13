%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function files = expandFileList(names, identifier)
  % Full names of the files named by text, wildcards or folders (folders are listed recursively).
  names = cellstr(convertStringsToChars(names));
  files = {};
  for k = 1:numel(names)
    name = names{k};
    if isfolder(name)
      found = folderFiles(name);
    else
      listing = dir(name);
      listing = listing(~[listing.isdir]);
      found = fullfile({listing.folder}, {listing.name});
    end
    if isempty(found)
      error(identifier, 'File not found: %s', name);
    end
    files = [files, found]; %#ok<AGROW>
  end
end
%=============================================================================
function files = folderFiles(folder)
  % Files of folder and of its subfolders.
  listing = dir(folder);
  files = {};
  for k = 1:numel(listing)
    if any(strcmp(listing(k).name, {'.', '..'}))
      continue
    end
    path = fullfile(folder, listing(k).name);
    if listing(k).isdir
      files = [files, folderFiles(path)]; %#ok<AGROW>
    else
      files{end + 1} = path; %#ok<AGROW>
    end
  end
end
%=============================================================================
