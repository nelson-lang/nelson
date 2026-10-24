%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sourceCaught = false;
try
  error('Nelson:test:rethrowSource', 'Rethrow source message.');
catch ME
  sourceCaught = true;
end
%=============================================================================
asserts.istrue(sourceCaught);
caught = false;
try
  rethrow(ME);
catch caughtException
  caught = true;
end
%=============================================================================
asserts.istrue(caught);
asserts.isequal(caughtException.identifier, 'Nelson:test:rethrowSource');
asserts.isequal(caughtException.message, 'Rethrow source message.');
%=============================================================================
