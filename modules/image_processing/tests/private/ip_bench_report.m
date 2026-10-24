%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ip_bench_report(name, elapsedSeconds, warnSeconds)
  disp([name, '_seconds=', num2str(elapsedSeconds)]);
  if nargin < 3 || isempty(warnSeconds)
    return
  end
  if ~(isscalar(warnSeconds) && isfinite(warnSeconds) && warnSeconds > 0)
    return
  end
  if elapsedSeconds <= warnSeconds
    return
  end
  msg = [name, ' exceeded ', num2str(warnSeconds), ' s'];
  disp(['bench_warning=', msg]);
  if strcmpi(getenv('NELSON_IMAGE_PROCESSING_BENCH_STRICT'), 'TRUE')
    error(msg);
  end
end
%=============================================================================
