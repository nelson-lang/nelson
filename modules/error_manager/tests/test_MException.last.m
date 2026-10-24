%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
MException.last('reset');
lastException = MException.last;
asserts.isequal(class(lastException), 'MException');
asserts.isequal(lastException.identifier, '');
asserts.isequal(lastException.message, '');
%=============================================================================
caught = false;
try
  error('Nelson:test:lastDirect', 'Last direct message.');
catch ME
  caught = true;
end
%=============================================================================
asserts.istrue(caught);
lastException = MException.last;
asserts.isequal(ME.identifier, 'Nelson:test:lastDirect');
asserts.isequal(ME.message, 'Last direct message.');
asserts.isequal(lastException.identifier, '');
asserts.isequal(lastException.message, '');
%=============================================================================
MException.last('reset');
%=============================================================================
