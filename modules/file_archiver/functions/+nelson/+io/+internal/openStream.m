%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function fid = openStream(codec, defaults, source, mode, varargin)
  % File identifier on compressed data: a file, uint8 bytes, or [] to write in memory.
  [encoding, options] = nelson.io.internal.fopenOptions(defaults, varargin{:});
  if isstring(source)
    source = char(source);
  end
  if nelson.io.internal.isUrl(source)
    if ~startsWith(char(mode), 'r')
      error('Nelson:io:urlReadOnly', 'A URL can only be opened for reading (mode ''r'').');
    end
    source = nelson.io.internal.urlBytes(source);
  end
  workers = 0;
  seekable = false;
  if isfield(options, 'Workers')
    workers = options.Workers;
  end
  if isfield(options, 'Seekable')
    seekable = logical(options.Seekable);
  end
  fid = __stream_fopen__(codec, source, char(mode), encoding, double(options.Level), ...
    double(options.MaxSize), nelson.io.internal.workersValue(workers), seekable);
end
%=============================================================================
