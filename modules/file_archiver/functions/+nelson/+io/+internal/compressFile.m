%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function destination = compressFile(codec, extension, defaults, source, varargin)
  % Compresses a file into source name + extension, or into the destination given first.
  args = varargin;
  destination = [char(source), extension];
  if mod(numel(args), 2) == 1
    destination = char(args{1});
    args(1) = [];
  end
  options = nelson.io.internal.nameValueOptions(defaults, args{:});
  workers = 0;
  if isfield(options, 'Workers')
    workers = options.Workers;
  end
  __stream_compressfile__(codec, char(source), destination, double(options.Level), ...
    nelson.io.internal.workersValue(workers));
end
%=============================================================================
