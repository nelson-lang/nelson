%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Profile a repeatable matrix computation and inspect the collected report.
profile('clear');
profile('on');
for k = 1:25
  singularValues = svd(hilb(30));
end
profile('off');
report = profile('info');
disp(report);
profile('clear');
