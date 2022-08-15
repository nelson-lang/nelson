%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = date()
  c = clock();
  months = ['Jan'; 'Feb'; 'Mar'; 'Apr'; 'May'; 'Jun'; 'Jul'; 'Aug'; 'Sep'; 'Oct'; 'Nov'; 'Dec'];
  d = sprintf('%.0f', c(3) + 100);
  t = sprintf('%s-%s-%s', d(2:3), months(c(2), :), sprintf('%.0f',c(1)));
end
%=============================================================================

