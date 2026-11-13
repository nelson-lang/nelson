%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = decompressData(codec, bytes, varargin)
  % Decompresses uint8 data of codec 'zstd' or 'gzip' into a uint8 row vector.
  defaults = struct('MaxSize', Inf, 'MaxCompressionRatio', Inf);
  options = nelson.io.internal.nameValueOptions(defaults, varargin{:});
  if ~isa(bytes, 'uint8')
    error('Nelson:io:invalidData', 'Data must be uint8.');
  end
  data = __stream_decompress__(codec, reshape(bytes, 1, []), double(options.MaxSize), ...
    double(options.MaxCompressionRatio));
end
%=============================================================================
