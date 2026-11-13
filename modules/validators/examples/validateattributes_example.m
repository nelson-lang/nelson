%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = [1 2 3 4];
validateattributes(x, {'numeric'}, {'row', 'nonempty', 'positive', 'finite'});
disp('x = [1 2 3 4] is a finite, positive, non-empty numeric row vector.');
% A rejected input raises a descriptive error.
try
  validateattributes([1 -2 3], {'numeric'}, {'positive'});
catch err
  disp(['[1 -2 3] rejected: ', err.message]);
end
%=============================================================================
