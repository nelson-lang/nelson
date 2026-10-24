%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function callback_emit(message)
  if ~isdeployed() || ~isfolder(ctfroot())
    error('Expected deployed state in application callback.');
  end
  fprintf('%s\n', message);
end
