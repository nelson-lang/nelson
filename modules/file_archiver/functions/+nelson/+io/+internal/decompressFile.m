%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function destination = decompressFile(codec, extension, source, varargin)
  % Decompresses a file into its name without extension, or into the destination given first.
  args = varargin;
  if mod(numel(args), 2) == 1
    destination = char(args{1});
    args(1) = [];
  elseif endsWith(char(source), extension, 'IgnoreCase', true)
    name = char(source);
    destination = name(1:end - strlength(extension));
  elseif strcmp(codec, 'gzip')
    error('Nelson:io:gzip:destinationExpected', 'A destination is needed when the source does not end with .gz.');
  else
    error('Nelson:io:zstd:destinationExpected', 'A destination is needed when the source does not end with .zst.');
  end
  defaults = struct('MaxSize', Inf, 'MaxCompressionRatio', Inf);
  options = nelson.io.internal.nameValueOptions(defaults, args{:});
  __stream_decompressfile__(codec, char(source), destination, double(options.MaxSize), ...
    double(options.MaxCompressionRatio));
end
%=============================================================================
