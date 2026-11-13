%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bytes = compressData(codec, defaults, data, varargin)
  % Compresses uint8 data or text (UTF-8) with codec 'zstd' or 'gzip'.
  options = nelson.io.internal.nameValueOptions(defaults, varargin{:});
  bytes = __stream_compress__(codec, nelson.io.internal.bytesFromData(data), double(options.Level), ...
    nelson.io.internal.workersValue(optionOrDefault(options, 'Workers', 0)));
end
%=============================================================================
function value = optionOrDefault(options, name, default)
  value = default;
  if isfield(options, name)
    value = options.(name);
  end
end
%=============================================================================
