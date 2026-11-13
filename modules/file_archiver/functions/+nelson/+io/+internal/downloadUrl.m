%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filename = downloadUrl(url)
  % Downloads url into a new temporary file that keeps the extension of the URL path.
  address = char(url);
  path = regexprep(address, '[?#].*$', '');
  [~, ~, ext] = fileparts(path);
  if endsWith(path, '.tar.gz', 'IgnoreCase', true)
    ext = '.tar.gz';
  elseif endsWith(path, '.tar.zst', 'IgnoreCase', true)
    ext = '.tar.zst';
  end
  filename = [tempname(), ext];
  websave(filename, address);
end
%=============================================================================
