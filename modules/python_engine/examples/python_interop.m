%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Exchange an array with Python and retrieve a computed result.
try
  environment = pyenv();
catch exception
  error(['This example requires a compatible Python installation: ', exception.message]);
end
if strlength(environment.Executable) == 0
  error('A Python installation is required for this example.');
end
values = [1, 2, 3, 4];
result = pyrun('result = sum(value * value for value in values)', 'result', 'values', values);
disp(['Sum of squares computed by Python: ', int2str(result)]);
