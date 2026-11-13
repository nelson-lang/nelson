%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = archiveSource(source, mode)
  % Archive source: uint8 bytes, [] (new in-memory archive), a file name or a URL (downloaded).
  s = struct('Target', [], 'InMemory', false, 'Bytes', uint8([]), 'Source', "", 'DownloadedFile', '');
  if isa(source, 'uint8')
    s.InMemory = true;
    s.Bytes = reshape(source, 1, []);
    s.Target = s.Bytes;
  elseif isnumeric(source) && isempty(source)
    s.InMemory = true;
  else
    s.Source = string(source);
    s.Target = char(s.Source);
    if nelson.io.internal.isUrl(source)
      if mode ~= "r"
        error('Nelson:io:urlReadOnly', 'A URL can only be opened for reading (mode ''r'').');
      end
      s.DownloadedFile = nelson.io.internal.downloadUrl(source);
      s.Target = s.DownloadedFile;
    end
  end
end
%=============================================================================
