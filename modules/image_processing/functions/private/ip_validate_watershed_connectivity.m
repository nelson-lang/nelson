%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function conn = ip_validate_watershed_connectivity(value, isVolume)
  if nargin < 2
    isVolume = false;
  end
  if ~(isnumeric(value) || islogical(value)) || ~isreal(value)
    ip_watershed_connectivity_error(isVolume);
  end
  if isscalar(value)
    conn = double(value);
    if ~isVolume && isfinite(conn) && (conn == 4 || conn == 8)
      return
    end
    if isVolume && isfinite(conn) && (conn == 6 || conn == 18 || conn == 26)
      return
    end
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  expectedSize = [3 3];
  if isVolume
    expectedSize = [3 3 3];
  end
  if ~isequal(size(value), expectedSize) || issparse(value)
    ip_watershed_connectivity_error(isVolume);
  end
  if any(~isfinite(double(value(:))))
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  nhood = double(value) ~= 0;
  if isVolume
    if isequal(nhood, ip_watershed_nhood3(6))
      conn = 6;
    elseif isequal(nhood, ip_watershed_nhood3(18))
      conn = 18;
    elseif all(nhood(:))
      conn = 26;
    else
      error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
    end
  else
    cross = logical([0 1 0; 1 1 1; 0 1 0]);
    if isequal(nhood, cross)
      conn = 4;
    elseif all(nhood(:))
      conn = 8;
    else
      error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
    end
  end
end
%=============================================================================
function nhood = ip_watershed_nhood3(conn)
  nhood = false(3, 3, 3);
  for p = 1:3
    for c = 1:3
      for r = 1:3
        d = abs(r - 2) + abs(c - 2) + abs(p - 2);
        if conn == 6
          nhood(r, c, p) = d <= 1;
        elseif conn == 18
          nhood(r, c, p) = d <= 2;
        else
          nhood(r, c, p) = true;
        end
      end
    end
  end
end
%=============================================================================
function ip_watershed_connectivity_error(isVolume)
  if isVolume
    error('Nelson:image_processing:connectivityNumericLogicalScalarArray', 'Connectivity must be a numeric/logical scalar or 3-by-3-by-3 array.');
  else
    error('Nelson:image_processing:connectivityNumericLogicalScalarMatrix', 'Connectivity must be a numeric/logical scalar or 3-by-3 matrix.');
  end
end
%=============================================================================
