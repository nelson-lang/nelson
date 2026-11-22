%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function hash = nmm_sidecar_sha256(sidecarFile)
  % Returns the hash alone from a ".sha256" sidecar, tolerating both the bare
  % "<hex>" form and the "sha256sum" two-column "<hex>  <name>" form.
  hash = strtrim(fileread(sidecarFile));
  separator = strfind(hash, ' ');
  if ~isempty(separator)
    hash = hash(1:separator(1) - 1);
  end
end
%=============================================================================
