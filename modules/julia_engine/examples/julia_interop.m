%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Exchange an array with Julia and retrieve a computed result.
try
  environment = jlenv();
catch exception
  error(['This example requires a compatible Julia installation: ', exception.message]);
end
if strlength(environment.Executable) == 0 || strlength(environment.Library) == 0
  error('A Julia installation is required for this example.');
end
values = [1, 2, 3, 4];
result = jlrun('result = sum(values .^ 2)', 'result', 'values', values);
disp(['Sum of squares computed by Julia: ', int2str(result)]);
