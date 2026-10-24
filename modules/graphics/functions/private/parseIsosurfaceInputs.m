%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, y, z, V, isovalue, colors, noshare, verbose] = parseIsosurfaceInputs(args)
  [args, noshare, verbose] = parseIsosurfaceOptions(args);
  if numel(args) <= 3
    [x, y, z, V, isovalue, colors] = parseDefaultIsosurfaceData(args);
    return
  end
  if numel(args) >= 4 && numel(args) <= 6
    [x, y, z, V, isovalue, colors] = parseExplicitIsosurfaceData(args);
    return
  end
  error('Nelson:tooManyInputs', 'Too many input arguments.');
end
%=============================================================================
