%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rejectStreamPropertyArguments(args)
  % stream2 and stream3 compute vertices and draw nothing, so they take no
  % line property. Saying so is clearer than letting a property name reach the
  % field parser, which would count it as one of the data arguments.
  for k = 1:numel(args)
    if (ischar(args{k}) || isStringScalar(args{k}))
      error('Nelson:graphics:unexpectedPropertyArgument', ...
        'Property arguments are not accepted here.');
    end
  end
end
%=============================================================================
