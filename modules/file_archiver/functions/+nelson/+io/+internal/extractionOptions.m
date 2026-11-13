%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [folder, options] = extractionOptions(args)
  % Optional output folder (first of an odd count) and the extraction limits.
  folder = pwd();
  if mod(numel(args), 2) == 1
    folder = char(args{1});
    args(1) = [];
  end
  defaults = struct('MaxExtractedSize', Inf, 'MaxCompressionRatio', Inf);
  options = nelson.io.internal.nameValueOptions(defaults, args{:});
  options.MaxExtractedSize = double(options.MaxExtractedSize);
  options.MaxCompressionRatio = double(options.MaxCompressionRatio);
end
%=============================================================================
