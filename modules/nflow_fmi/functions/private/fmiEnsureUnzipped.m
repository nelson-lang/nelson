%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% [dir, tempToClean] = fmiEnsureUnzipped(fmuOrDir)
%
% Resolves an FMU location to an extracted directory. If fmuOrDir is already a
% directory it is returned as-is (tempToClean empty). If it is a .fmu archive
% it is extracted with the ZIP-slip-hardened unzip into a fresh temporary
% directory, whose path is returned as tempToClean for the caller to remove.
%=============================================================================
function [fmuDir, tempToClean] = fmiEnsureUnzipped(fmuOrDir)
  tempToClean = '';
  if iswasm()
    % The Worker owns the registered component; its path is an opaque identifier.
    fmuDir = fmuOrDir;
    return;
  end
  if isdir(fmuOrDir)
    fmuDir = fmuOrDir;
    return;
  end
  if ~isfile(fmuOrDir)
    error('Nelson:nflow_fmi:pathNotFound', 'FMU path is neither a directory nor an existing file: %s', fmuOrDir);
  end
  fmuDir = [tempname(), '_fmu'];
  mkdir(fmuDir);
  try
    unzip(fmuOrDir, fmuDir);
  catch e
    rmdir(fmuDir, 's');
    error('Nelson:nflow_fmi:extractionFailed', 'Failed to extract FMU archive ''%s'': %s', fmuOrDir, e.message);
  end
  tempToClean = fmuDir;
end
%=============================================================================
