%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/error_manager/tests']);
wasCaught = false;
try
  rethrow_private_function();
catch ex
  wasCaught = true;
  asserts.isequal(ex.identifier, 'Nelson:test:privateRethrow')
  asserts.isequal(ex.message, 'Private rethrow message.')
  asserts.istrue(any(strcmp({ex.stack.name}, 'rethrow_private_helper')))
  asserts.istrue(any(strcmp({ex.stack.name}, 'rethrow_private_function')))
end
asserts.istrue(wasCaught)
%=============================================================================
