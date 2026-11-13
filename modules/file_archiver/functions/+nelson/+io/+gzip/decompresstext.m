%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = decompresstext(bytes, varargin)
  % Decompresses gzip data and decodes it as text (UTF-8 unless Encoding is given).
  defaults = struct('Encoding', 'UTF-8', 'MaxSize', Inf, 'MaxCompressionRatio', Inf);
  options = nelson.io.internal.nameValueOptions(defaults, varargin{:});
  txt = native2unicode(nelson.io.gzip.decompress(bytes, 'MaxSize', options.MaxSize, ...
    'MaxCompressionRatio', options.MaxCompressionRatio), char(options.Encoding));
end
%=============================================================================
