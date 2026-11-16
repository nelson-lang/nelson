%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Install an assistant skill package into the user skills directory. The source
% is a local .nmz/.zip file, or the name of a package in the catalog (which is
% then downloaded first). A .nmz is a zip archive containing a SKILL.md.
function install(source)
  narginchk(1, 1);
  nargoutchk(0, 0);
  source = char(source);
  localFile = source;
  downloaded = '';
  if ~isfile(localFile)
    localFile = downloadByName(source);
    downloaded = localFile;
  end
  [~, base, ~] = fileparts(localFile);
  target = [skillsDir(), '/', base];
  if isfolder(target)
    rmdir(target, 's');
  end
  unzip(localFile, target);
  if ~isempty(downloaded) && isfile(downloaded)
    rmfile(downloaded);
  end
end
%=============================================================================
function file = downloadByName(name)
  entries = nelson.ai.marketplace.search(name);
  index = [];
  for k = 1:numel(entries)
    if strcmp(entries(k).name, name)
      index = k;
      break
    end
  end
  if isempty(index)
    error('Nelson:ai:skillNotFound', 'No skill package named "%s" in the catalog.', name);
  end
  file = [tempname(), '.nmz'];
  websave(file, entries(index).url);
end
%=============================================================================
